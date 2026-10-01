-- | The page: an editor, the engine it speaks to, and what came back.
-- |
-- | The same buffer can be sent to Haskell Tidal or to purerl-tidal; that is
-- | the point of the page. Hush silences both, since a flipped buffer can
-- | leave the other engine playing, and so does Panic from any Atlantis page.
module Limulus.App (component) where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array (cons, snoc, take, uncons)
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String (Pattern(..), stripPrefix)
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
  }

data Action
  = Init
  | Eval String
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

component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ ->
      { engine: Ghci, ghci: Off, socket: Nothing, purerlUp: false, log: []
      , nextId: 0, pending: [], listener: Nothing }
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
    bus <- liftEffect Bus.open
    liftEffect $ Bus.onMessage bus (HS.notify listener <<< FromBus)
    handleAction Connect
    void $ H.fork $ H.liftAff $ forever do
      liftEffect (HS.notify listener PollGhci)
      delay (Milliseconds 1500.0)

  Eval block -> do
    st <- H.get
    let id = st.nextId
    H.modify_ _ { nextId = id + 1, log = take 60 (cons { id, engine: st.engine, block, reply: Nothing } st.log) }
    case st.engine of
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

  PurerlUp -> H.modify_ _ { purerlUp = true }

  -- Unanswered blocks will not be answered now; try again in a while.
  PurerlDown -> do
    st <- H.get
    for_ st.pending \id -> handleAction (Answered id { ok: false, out: "purerl-tidal went away" })
    H.modify_ _ { purerlUp = false, socket = Nothing, pending = [] }
    void $ H.fork do
      H.liftAff (delay (Milliseconds 3000.0))
      handleAction Connect

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
sendPurerl id block = do
  st <- H.get
  sent <- case st.socket of
    Just ws -> liftEffect (Engine.send ws (Engine.purerlBlock block))
    Nothing -> pure false
  if sent then H.modify_ \s -> s { pending = snoc s.pending id }
  else handleAction (Answered id { ok: false, out: "purerl-tidal is not connected (ws :3012)" })

-- | purerl-tidal's refusals start ERR or ERROR.
isErr :: String -> Boolean
isErr text = isJust (stripPrefix (Pattern "ERR") text)
