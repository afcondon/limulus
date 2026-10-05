-- | What each stage object's block last agreed with the stage on, by the
-- | object's key, kept beside the buffer. A block that still says this is
-- | behind the stage (another page moved on while Limulus was closed), not an
-- | edit in hand, so it can be brought up to date.
module Limulus.Synced (load, save) where

import Prelude

import Effect (Effect)
import Foreign.Object (Object)

foreign import load :: Effect (Object String)
foreign import save :: Object String -> Effect Unit
