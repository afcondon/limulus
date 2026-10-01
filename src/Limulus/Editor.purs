-- | The editor: a CodeMirror buffer that hands over blocks.
-- |
-- | It knows what Tidal's own editors know and no more: a block is the
-- | selection, or the run of non-blank lines around the cursor; Cmd-Enter
-- | (or Shift-Enter) sends it, Cmd-. hushes. Which engine hears it is not its
-- | business. The buffer is kept in this browser between visits.
module Limulus.Editor
  ( Editor
  , Handlers
  , create
  , focus
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn3, runEffectFn1, runEffectFn3)
import Web.DOM (Element)

foreign import data Editor :: Type

type Handlers =
  { onEval :: String -> Effect Unit
  , onHush :: Effect Unit
  }

foreign import _create :: EffectFn3 Element String Handlers Editor
foreign import _focus :: EffectFn1 Editor Unit

-- | Mount an editor in the element, starting from the text given unless this
-- | browser has a buffer from last time.
create :: Element -> String -> Handlers -> Effect Editor
create = runEffectFn3 _create

focus :: Editor -> Effect Unit
focus = runEffectFn1 _focus
