-- | The two engines a block can go to, and how to reach each.
-- |
-- | **Haskell Tidal** runs in GHCi behind this app's own server, which types
-- | each block in and returns what GHCi printed. **purerl-tidal** is reached
-- | over its WebSocket on :3012, as every Atlantis page reaches it; it answers
-- | each frame with one reply frame, in order.
module Limulus.Engine
  ( Engine(..)
  , engineName
  , Reply
  , GhciState(..)
  , ghciEval
  , ghciRestart
  , ghciStatus
  , Socket
  , SocketCallbacks
  , purerlUrl
  , connect
  , send
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn2, runEffectFn2)

data Engine = Ghci | Purerl

derive instance Eq Engine

engineName :: Engine -> String
engineName = case _ of
  Ghci -> "Tidal 1.10 · GHCi"
  Purerl -> "purerl-tidal"

-- | What an engine said about one block, and whether it took it.
type Reply = { ok :: Boolean, out :: String }

data GhciState = Off | Booting | Ready | Unreachable

derive instance Eq GhciState

foreign import _ghciEval :: String -> Effect (Promise Reply)
foreign import _ghciRestart :: Effect (Promise { ok :: Boolean })
foreign import _ghciStatus :: Effect (Promise String)

ghciEval :: String -> Aff Reply
ghciEval = toAffE <<< _ghciEval

ghciRestart :: Aff Unit
ghciRestart = void (toAffE _ghciRestart)

ghciStatus :: Aff GhciState
ghciStatus = toAffE _ghciStatus <#> case _ of
  "off" -> Off
  "booting" -> Booting
  "ready" -> Ready
  _ -> Unreachable

foreign import data Socket :: Type

type SocketCallbacks =
  { onOpen :: Effect Unit
  , onMessage :: String -> Effect Unit
  , onClose :: Effect Unit
  }

purerlUrl :: String
purerlUrl = "ws://localhost:3012/ws"

foreign import _connect :: EffectFn2 String SocketCallbacks Socket
foreign import _send :: EffectFn2 Socket String Boolean

connect :: String -> SocketCallbacks -> Effect Socket
connect = runEffectFn2 _connect

-- | False when the socket is not open, so nothing was sent.
send :: Socket -> String -> Effect Boolean
send = runEffectFn2 _send
