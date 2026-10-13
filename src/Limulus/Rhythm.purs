-- | A Balistes rhythm dropped on a `balistes $` line: its word joins the
-- | line's alternation, so dragging rhythms in one after another builds
-- | `balistes $ "<a b c>"` (each a bar; the rig plays them by name).
module Limulus.Rhythm
  ( balistesLine
  , joinRhythm
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), indexOf, lastIndexOf, trim)
import Data.String.CodeUnits as CU

-- | A new line playing one rhythm.
balistesLine :: String -> String
balistesLine word = "balistes $ \"<" <> word <> ">\""

-- | The block with `word` added to its pattern, if the block is a
-- | `balistes $ "…"` line: before the `>` of an alternation (`<a b>` →
-- | `<a b word>`), else around what is there (`a` → `<a word>`, `a b` →
-- | `<[a b] word>`, so a sequence stays one step of the alternation).
-- | Nothing for any other block.
joinRhythm :: String -> String -> Maybe String
joinRhythm word block = do
  let t = trim block
  rest <- CU.stripPrefix (Pattern "balistes") t
  dollar <- indexOf (Pattern "$") rest
  open <- indexOf (Pattern "\"") rest
  close <- lastIndexOf (Pattern "\"") rest
  if dollar > open || close <= open then Nothing else do
    let
      inner = trim (CU.slice (open + 1) close rest)
      joined
        | inner == "" = "<" <> word <> ">"
        | CU.take 1 inner == "<" && CU.takeRight 1 inner == ">" =
            trim (CU.dropRight 1 inner) <> " " <> word <> ">"
        | single inner = "<" <> inner <> " " <> word <> ">"
        | otherwise = "<[" <> inner <> "] " <> word <> ">"
    pure ("balistes" <> CU.take (open + 1) rest <> joined <> CU.drop close rest)
  where
  single s = indexOf (Pattern " ") s == Nothing && indexOf (Pattern "[") s == Nothing
    && indexOf (Pattern "{") s == Nothing && indexOf (Pattern "(") s == Nothing
