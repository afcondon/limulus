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
  -- Vetula's cards as the stage holds them (card number → its line).
  , cards :: Map Int String
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
      , nextId: 0, pending: [], listener: Nothing, editor: Nothing, cards: Map.empty }
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
        , HH.button
            [ HP.class_ (H.ClassName "hush"), HE.onClick \_ -> Hush, HP.title "Silence both engines (Cmd-.)" ]
            [ HH.text "hush" ]
        ]
    , HH.main_
        [ HH.div [ HP.class_ (H.ClassName "editor"), HP.ref editorRef ] []
        , HH.aside [ HP.class_ (H.ClassName "log") ] (map entry st.log)
        ]
    , HH.footer_
        [ HH.text "⌘↵ evaluate block · ⌘. hush both · "
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

  Eval b | Just card <- Stage.cardLine b.text -> evalCard b card

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

-- | Evaluate a card block: write the card to the stage. A `vetula $` block
-- | becomes a new card, numbered here, and its head is rewritten to say so.
evalCard :: forall o. Editor.Block -> Stage.CardLine -> M o Unit
evalCard b card = do
  st <- H.get
  n <- case card.card of
    Just n -> pure n
    Nothing -> do
      taken <- case st.editor of
        Just ed -> liftEffect $ filterA (\k -> isJust <$> Editor.findBlock ed ("v" <> show k))
                     (range 1 (Map.size st.cards + 2))
        Nothing -> pure []
      let n = Stage.freeCard st.cards (\k -> elem k taken)
      for_ st.editor \ed -> for_ (indexOf (Pattern "$") b.text) \at ->
        liftEffect $ Editor.replace ed b.from (b.from + at) ("v" <> show n <> " ")
      pure n
  let id = st.nextId
  H.modify_ \s -> s
    { nextId = id + 1
    , log = take 60 (cons { id, engine: Purerl, block: "v" <> show n <> " $ " <> card.body, reply: Nothing } s.log)
    , cards = Map.insert n card.body s.cards
    }
  sendLine id ("stage-text " <> Stage.cardKey n <> " " <> card.body)

-- | A stage frame about a card. A card written elsewhere replaces its block
-- | only if the block still says what the stage last said, so an edit in hand
-- | is never overwritten; it is noted instead, and evaluating it wins.
stageFrame :: forall o. Stage.StageFrame -> M o Unit
stageFrame = case _ of
  Stage.Table cards -> H.modify_ _ { cards = cards }
  Stage.Written n text -> do
    st <- H.get
    let prev = Map.lookup n st.cards
    H.modify_ _ { cards = maybe (Map.delete n st.cards) (\t -> Map.insert n t st.cards) text }
    for_ st.editor \ed -> do
      mblock <- liftEffect (Editor.findBlock ed ("v" <> show n))
      for_ mblock \blk -> case text of
        Nothing -> note n "removed in Vetula; this block no longer names a card" false
        Just t
          | Stage.bodyOf blk.text == t -> pure unit
          | Just (Stage.bodyOf blk.text) == prev -> liftEffect (Editor.replace ed blk.from blk.to ("v" <> show n <> " $ " <> t))
          -- what the block was last in step with is unknown (it was refused):
          -- keep the typing, to be fixed and evaluated again
          | prev == Nothing -> pure unit
          | otherwise -> note n "changed in Vetula; this block differs, so it was left alone (evaluate it to make yours the card)" false
  Stage.Open n -> do
    st <- H.get
    for_ st.editor \ed -> do
      mblock <- liftEffect (Editor.findBlock ed ("v" <> show n))
      case mblock, Map.lookup n st.cards of
        Just blk, _ -> liftEffect (Editor.reveal ed blk.from blk.to)
        Nothing, Just t -> liftEffect (Editor.append ed ("v" <> show n <> " $ " <> t))
        Nothing, Nothing -> note n "Vetula asked to show it, but the stage has no such card" false
  -- A refused block is left as typed: forget what the stage said for it, so
  -- the card's real line, republished by Vetula, does not overwrite it.
  Stage.Rejected n reason -> do
    H.modify_ \s -> s { cards = Map.delete n s.cards }
    note n (reason <> "; your block is kept as typed") false

-- | A line in the log about card `n` that answers no block.
note :: forall o. Int -> String -> Boolean -> M o Unit
note n out ok = H.modify_ \s -> s
  { nextId = s.nextId + 1
  , log = take 60 (cons { id: s.nextId, engine: Purerl, block: "v" <> show n <> " · Vetula", reply: Just { ok, out } } s.log)
  }

-- | purerl-tidal's refusals start ERR or ERROR.
isErr :: String -> Boolean
isErr text = isJust (stripPrefix (Pattern "ERR") text)
