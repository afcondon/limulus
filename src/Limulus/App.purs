-- | The page: an editor, the engine it speaks to, and what came back.
-- |
-- | The same buffer can be sent to Haskell Tidal or to purerl-tidal; that is
-- | the point of the page. Hush silences both, since a flipped buffer can
-- | leave the other engine playing, and so does Panic from any Atlantis page.
-- |
-- | Vetula's cards are lines here too (`v3 $ …`, `Limulus.Stage`): the page
-- | subscribes to the rig's stage, writes a card when its block is evaluated,
-- | and keeps a card's block in step when the card changes in Vetula.
module Limulus.App (component) where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array (cons, elem, filterA, range, snoc, take, uncons)
import Data.Foldable (for_, traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String (Pattern(..), indexOf, stripPrefix)
import Limulus.Stage as Stage
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Binnacle.TabBus as Bus
import Limulus.Editor as Editor
import Limulus.Engine (Engine(..), GhciState(..), Reply, Socket, engineName)
import Limulus.Engine as Engine
import Web.HTML.HTMLElement as HTMLElement

type Entry =
  { id :: Int
  , engine :: Engine
  , block :: String
  , reply :: Maybe Reply
  }

type State =
  { engine :: Engine
  , ghci :: GhciState
  , socket :: Maybe Socket
  , purerlUp :: Boolean
  , log :: Array Entry
  , nextId :: Int
  -- Entries sent to purerl-tidal and not yet answered, oldest first: it
  -- answers every frame, in order.
  , pending :: Array Int
  , listener :: Maybe (HS.Listener Action)
  , editor :: Maybe Editor.Editor
  -- The stage objects Limulus edits (Vetula's cards, Conspicillum's cloud),
  -- as the stage holds them.
  , objects :: Map Stage.Obj String
  }

data Action
  = Init
  | Eval Editor.Block
  | Hush
  | SetEngine Engine
  | RestartGhci
  | PollGhci
  | Connect
  | PurerlUp
  | PurerlDown
  | PurerlSaid String
  | Answered Int Reply
  | FromBus Bus.Msg
  | SetEditor Editor.Editor

component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ ->
      { engine: Ghci, ghci: Off, socket: Nothing, purerlUp: false, log: []
      , nextId: 0, pending: [], listener: Nothing, editor: Nothing, objects: Map.empty }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Init }
  }

starter :: String
starter = """-- Text in, sound out. Cmd-Enter sends the block under the cursor,
-- Cmd-. hushes. The same lines go to either engine: that is the test.

d1 $ s "bd*4"

d2 $ s "~ hh*2 ~ hh" # gain 0.9

d1 $ fast 2 $ s "bd sn" # n "0 1"

d1 silence

hush
"""

-- ---------------------------------------------------------------- render

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.div [ HP.class_ (H.ClassName "page") ]
    [ HH.header [ HP.class_ (H.ClassName "bar") ]
        [ HH.h1_ [ HH.text "limulus" ]
        , HH.span [ HP.class_ (H.ClassName "motto") ] [ HH.text "text in, sound out" ]
        , HH.div [ HP.class_ (H.ClassName "engines") ]
            [ engineButton st Ghci (ghciLamp st.ghci)
            , engineButton st Purerl (if st.purerlUp then "up" else "down")
            ]
        , HH.a
            [ HP.class_ (H.ClassName "cheats"), HP.href "cheatsheet.html", HP.target "limulus-cheatsheet"
            , HP.title "What the rig adds to Tidal: drums, Odonus moves, Vetula cards, scales"
            ]
            [ HH.text "cheatsheet" ]
        , HH.button
            [ HP.class_ (H.ClassName "hush"), HE.onClick \_ -> Hush, HP.title "Silence everything: both engines and every machine on the rig (Cmd-.)" ]
            [ HH.text "hush" ]
        ]
    , HH.main_
        [ HH.div [ HP.class_ (H.ClassName "editor"), HP.ref editorRef ] []
        , HH.aside [ HP.class_ (H.ClassName "log") ] (map entry st.log)
        ]
    , HH.footer_
        [ HH.text "⌘↵ evaluate block · ⌘. hush everything · "
        , HH.button [ HP.class_ (H.ClassName "link"), HE.onClick \_ -> RestartGhci ]
            [ HH.text "restart GHCi" ]
        ]
    ]

engineButton :: forall m. State -> Engine -> String -> H.ComponentHTML Action () m
engineButton st e lamp =
  HH.button
    [ HP.classes (map H.ClassName ([ "engine", "lamp-" <> lamp ] <> if st.engine == e then [ "on" ] else []))
    , HE.onClick \_ -> SetEngine e
    ]
    [ HH.span [ HP.class_ (H.ClassName "lamp") ] [], HH.text (engineName e) ]

ghciLamp :: GhciState -> String
ghciLamp = case _ of
  Ready -> "up"
  Booting -> "booting"
  _ -> "down"

entry :: forall m. Entry -> H.ComponentHTML Action () m
entry e =
  HH.div [ HP.classes (map H.ClassName [ "entry", status ]) ]
    [ HH.div [ HP.class_ (H.ClassName "who") ] [ HH.text (engineName e.engine) ]
    , HH.pre [ HP.class_ (H.ClassName "block") ] [ HH.text e.block ]
    , HH.pre [ HP.class_ (H.ClassName "reply") ] [ HH.text (maybe "…" _.out e.reply) ]
    ]
  where
  status = case e.reply of
    Nothing -> "waiting"
    Just r -> if r.ok then "ok" else "err"

editorRef :: H.RefLabel
editorRef = H.RefLabel "editor"

-- ---------------------------------------------------------------- eval

type M o = H.HalogenM State Action () o Aff

handleAction :: forall o. Action -> M o Unit
handleAction = case _ of
  Init -> do
    { emitter, listener } <- liftEffect HS.create
    void $ H.subscribe emitter
    H.modify_ _ { listener = Just listener }
    H.getHTMLElementRef editorRef >>= traverse_ \el -> liftEffect do
      ed <- Editor.create (HTMLElement.toElement el) starter
        { onEval: HS.notify listener <<< Eval, onHush: HS.notify listener Hush }
      Editor.focus ed
      HS.notify listener (SetEditor ed)
    bus <- liftEffect Bus.open
    liftEffect $ Bus.onMessage bus (HS.notify listener <<< FromBus)
    handleAction Connect
    void $ H.fork $ H.liftAff $ forever do
      liftEffect (HS.notify listener PollGhci)
      delay (Milliseconds 1500.0)

  SetEditor ed -> H.modify_ _ { editor = Just ed }

  Eval b | Just sl <- Stage.stageLine b.text -> evalObj b sl

  Eval { text: block } -> do
    st <- H.get
    let
      id = st.nextId
      engine = if Engine.machineLine block then Purerl else st.engine
    H.modify_ _ { nextId = id + 1, log = take 60 (cons { id, engine, block, reply: Nothing } st.log) }
    case engine of
      Ghci -> do
        reply <- H.liftAff (Engine.ghciEval block)
        handleAction (Answered id reply)
      Purerl -> sendPurerl id block

  Hush -> do
    void $ H.fork $ void $ H.liftAff (Engine.ghciEval "hush")
    st <- H.get
    for_ st.socket \ws -> liftEffect (Engine.send ws (Engine.purerlBlock "hush"))
    H.modify_ \s -> s { pending = [] }

  SetEngine e -> H.modify_ _ { engine = e }

  RestartGhci -> do
    H.liftAff Engine.ghciRestart
    H.modify_ _ { ghci = Booting }

  PollGhci -> do
    g <- H.liftAff Engine.ghciStatus
    H.modify_ _ { ghci = g }

  Connect -> do
    st <- H.get
    for_ st.listener \l -> do
      ws <- liftEffect $ Engine.connect Engine.purerlUrl
        { onOpen: HS.notify l PurerlUp
        , onMessage: HS.notify l <<< PurerlSaid
        , onClose: HS.notify l PurerlDown
        }
      H.modify_ _ { socket = Just ws }

  -- Subscribe to the stage's text objects: its answer and later writes arrive
  -- as stage frames, not as replies.
  PurerlUp -> do
    H.modify_ _ { purerlUp = true }
    st <- H.get
    for_ st.socket \ws -> liftEffect (Engine.send ws "stage-text-subscribe")

  -- Unanswered blocks will not be answered now; try again in a while.
  PurerlDown -> do
    st <- H.get
    for_ st.pending \id -> handleAction (Answered id { ok: false, out: "purerl-tidal went away" })
    H.modify_ _ { purerlUp = false, socket = Nothing, pending = [] }
    void $ H.fork do
      H.liftAff (delay (Milliseconds 3000.0))
      handleAction Connect

  PurerlSaid text | Stage.isStageFrame text -> for_ (Stage.readFrame text) stageFrame

  PurerlSaid text -> do
    st <- H.get
    for_ (uncons st.pending) \{ head, tail } -> do
      H.modify_ _ { pending = tail }
      handleAction (Answered head { ok: not (isErr text), out: text })

  Answered id reply ->
    H.modify_ \s -> s { log = map (\e -> if e.id == id then e { reply = Just reply } else e) s.log }

  FromBus msg -> case msg of
    Bus.Panic -> handleAction Hush
    _ -> pure unit

sendPurerl :: forall o. Int -> String -> M o Unit
sendPurerl id block = sendLine id (Engine.purerlBlock block)

-- | Send one frame for log entry `id`, whose reply answers it.
sendLine :: forall o. Int -> String -> M o Unit
sendLine id line = do
  st <- H.get
  sent <- case st.socket of
    Just ws -> liftEffect (Engine.send ws line)
    Nothing -> pure false
  if sent then H.modify_ \s -> s { pending = snoc s.pending id }
  else handleAction (Answered id { ok: false, out: "purerl-tidal is not connected (ws :3012)" })

-- | Evaluate a stage block: write the object to the stage. A `vetula $` block
-- | becomes a new card, numbered here, and its head is rewritten to say so.
evalObj :: forall o. Editor.Block -> Stage.StageLine -> M o Unit
evalObj b sl = do
  st <- H.get
  obj <- case sl.obj of
    Just obj -> pure obj
    Nothing -> do
      taken <- case st.editor of
        Just ed -> liftEffect $ filterA (\k -> isJust <$> Editor.findBlock ed ("v" <> show k))
                     (range 1 (Map.size st.objects + 2))
        Nothing -> pure []
      let obj = Stage.Card (Stage.freeCard st.objects (\k -> elem k taken))
      for_ st.editor \ed -> for_ (indexOf (Pattern "$") b.text) \at ->
        liftEffect $ Editor.replace ed b.from (b.from + at) (Stage.headOf obj <> " ")
      pure obj
  let id = st.nextId
  H.modify_ \s -> s
    { nextId = id + 1
    , log = take 60 (cons { id, engine: Purerl, block: Stage.headOf obj <> " $ " <> sl.body, reply: Nothing } s.log)
    , objects = Map.insert obj sl.body s.objects
    }
  sendLine id ("stage-text " <> Stage.objKey obj <> " " <> sl.body)

-- | A stage frame about an object. One written elsewhere replaces its block
-- | only if the block still says what the stage last said, so an edit in hand
-- | is never overwritten; it is noted instead, and evaluating it wins.
stageFrame :: forall o. Stage.StageFrame -> M o Unit
stageFrame = case _ of
  Stage.Table objects -> H.modify_ _ { objects = objects }
  Stage.Written obj text -> do
    st <- H.get
    let prev = Map.lookup obj st.objects
    H.modify_ _ { objects = maybe (Map.delete obj st.objects) (\t -> Map.insert obj t st.objects) text }
    for_ st.editor \ed -> do
      mblock <- liftEffect (Editor.findBlock ed (Stage.headOf obj))
      for_ mblock \blk -> case text of
        Nothing -> note obj ("removed in " <> Stage.ownerOf obj <> "; this block no longer names anything") false
        Just t
          | Stage.bodyOf blk.text == t -> pure unit
          | Just (Stage.bodyOf blk.text) == prev -> liftEffect (Editor.replace ed blk.from blk.to (Stage.blockOf obj t))
          -- what the block was last in step with is unknown (it was refused):
          -- keep the typing, to be fixed and evaluated again
          | prev == Nothing -> pure unit
          | otherwise -> note obj ("changed in " <> Stage.ownerOf obj <> "; this block differs, so it was left alone (evaluate it to make yours the one)") false
  Stage.Open obj -> do
    st <- H.get
    for_ st.editor \ed -> do
      mblock <- liftEffect (Editor.findBlock ed (Stage.headOf obj))
      case mblock, Map.lookup obj st.objects of
        Just blk, _ -> liftEffect (Editor.reveal ed blk.from blk.to)
        Nothing, Just t -> liftEffect (Editor.append ed (Stage.blockOf obj t))
        Nothing, Nothing -> note obj (Stage.ownerOf obj <> " asked to show it, but the stage does not have it") false
  -- A refused block is left as typed: forget what the stage said for it, so
  -- the real line, republished by its page, does not overwrite it.
  Stage.Rejected obj reason -> do
    H.modify_ \s -> s { objects = Map.delete obj s.objects }
    note obj (reason <> "; your block is kept as typed") false

-- | A line in the log about an object, answering no block.
note :: forall o. Stage.Obj -> String -> Boolean -> M o Unit
note obj out ok = H.modify_ \s -> s
  { nextId = s.nextId + 1
  , log = take 60 (cons { id: s.nextId, engine: Purerl, block: Stage.headOf obj <> " · " <> Stage.ownerOf obj, reply: Just { ok, out } } s.log)
  }

-- | purerl-tidal's refusals start ERR or ERROR.
isErr :: String -> Boolean
isErr text = isJust (stripPrefix (Pattern "ERR") text)
