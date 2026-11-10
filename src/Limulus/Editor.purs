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
  , findBlocks
  , replace
  , append
  , reveal
  , blockAround
  , insertAt
  , embedded
  , Active
  , setActive
  , clearActive
  , active
  ) where

import Prelude

import Data.Array as Array
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
  -- a saved thing of one of `kinds` dropped at `pos` (Limulus.Drop)
  , kinds :: Array String
  , onDrop :: { kind :: String, payload :: String, pos :: Int } -> Effect Unit
  -- "take" pressed on a behind block: the object's stage key
  , onTake :: String -> Effect Unit
  }

-- | A stage object's active block: the one last evaluated, or found to agree
-- | with the stage. `behind`: a page has changed the object since.
type Active = { text :: String, from :: Int, to :: Int, behind :: Boolean }

foreign import _setActive :: EffectFn4 Editor String Int Boolean Unit
foreign import _clearActive :: EffectFn2 Editor String Unit
foreign import _active :: EffectFn3 Editor String String (Nullable Active)

-- | Make the block starting at a position the object's (by stage key) active
-- | one, and say whether it is behind.
setActive :: Editor -> String -> Int -> Boolean -> Effect Unit
setActive = runEffectFn4 _setActive

clearActive :: Editor -> String -> Effect Unit
clearActive = runEffectFn2 _clearActive

-- | The object's active block (stage key, head), while it is still one.
active :: Editor -> String -> String -> Effect (Maybe Active)
active ed key head = toMaybe <$> runEffectFn3 _active ed key head

foreign import _create :: EffectFn3 Element String Handlers Editor
foreign import _focus :: EffectFn1 Editor Unit
foreign import _findBlocks :: EffectFn2 Editor String (Array Block)
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

-- | The first block whose first line starts `head $` (`findBlock ed "Q"`).
findBlock :: Editor -> String -> Effect (Maybe Block)
findBlock ed head = Array.head <$> findBlocks ed head

-- | Every block whose first line starts `head $`, in buffer order.
findBlocks :: Editor -> String -> Effect (Array Block)
findBlocks = runEffectFn2 _findBlocks

replace :: Editor -> Int -> Int -> String -> Effect Unit
replace = runEffectFn4 _replace

append :: Editor -> String -> Effect Unit
append = runEffectFn2 _append

reveal :: Editor -> Int -> Int -> Effect Unit
reveal = runEffectFn3 _reveal

-- | Whether this Limulus is embedded in a machine's page (`?embed`), rather
-- | than a tab of its own.
foreign import _embedded :: Effect Boolean

embedded :: Effect Boolean
embedded = _embedded
