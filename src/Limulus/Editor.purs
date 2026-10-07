-- | The editor: a CodeMirror buffer that hands over blocks.
-- |
-- | It knows what Tidal's own editors know and no more: a block is the
-- | selection, or the run of non-blank lines around the cursor; Cmd-Enter
-- | (or Shift-Enter) sends it, Cmd-. hushes. Which engine hears it is not its
-- | business. The buffer is kept in this browser between visits.
-- |
-- | It can also find a block by its head (`v3 $`), replace text, and add a
-- | block at the end: what the stage needs to keep a card's line in step with
-- | the card (docs/kb/plans/text-on-the-stage.md).
module Limulus.Editor
  ( Editor
  , Block
  , Handlers
  , create
  , focus
  , findBlock
  , replace
  , append
  , reveal
  , blockAround
  , insertAt
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4)
import Web.DOM (Element)

foreign import data Editor :: Type

-- | A block's text and where it sits in the buffer.
type Block = { text :: String, from :: Int, to :: Int }

type Handlers =
  { onEval :: Block -> Effect Unit
  , onHush :: Effect Unit
  , onDropProgression :: { name :: String, pos :: Int } -> Effect Unit
  }

foreign import _create :: EffectFn3 Element String Handlers Editor
foreign import _focus :: EffectFn1 Editor Unit
foreign import _findBlock :: EffectFn2 Editor String (Nullable Block)
foreign import _replace :: EffectFn4 Editor Int Int String Unit
foreign import _append :: EffectFn2 Editor String Unit
foreign import _reveal :: EffectFn3 Editor Int Int Unit
foreign import _blockAround :: EffectFn2 Editor Int (Nullable Block)
foreign import _insertAt :: EffectFn4 Editor Int String Boolean Unit

-- | The block around a position (`Nothing` on a blank line).
blockAround :: Editor -> Int -> Effect (Maybe Block)
blockAround ed pos = toMaybe <$> runEffectFn2 _blockAround ed pos

-- | Insert text at a position and select it; `true`: as a block of its own.
insertAt :: Editor -> Int -> String -> Boolean -> Effect Unit
insertAt = runEffectFn4 _insertAt

-- | Mount an editor in the element, starting from the text given unless this
-- | browser has a buffer from last time.
create :: Element -> String -> Handlers -> Effect Editor
create = runEffectFn3 _create

focus :: Editor -> Effect Unit
focus = runEffectFn1 _focus

-- | The block whose first line starts `head $` (`findBlock ed "v3"`).
findBlock :: Editor -> String -> Effect (Maybe Block)
findBlock ed head = toMaybe <$> runEffectFn2 _findBlock ed head

replace :: Editor -> Int -> Int -> String -> Effect Unit
replace = runEffectFn4 _replace

append :: Editor -> String -> Effect Unit
append = runEffectFn2 _append

reveal :: Editor -> Int -> Int -> Effect Unit
reveal = runEffectFn3 _reveal
