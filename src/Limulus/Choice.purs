-- | Which engine Limulus sends Tidal to, as the Dashboard sets it (and as
-- | Limulus's own buttons set it): "architeuthis" or "ghci".
module Limulus.Choice (load, save, onChange) where

import Prelude

import Effect (Effect)

foreign import load :: Effect String
foreign import save :: String -> Effect Unit
foreign import onChange :: (String -> Effect Unit) -> Effect Unit
