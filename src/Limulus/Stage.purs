-- | **Cards as lines.** A Vetula card is a text object on the rig's stage,
-- | `vetula/v3`, holding the card's line (docs/kb/plans/text-on-the-stage.md).
-- | In Limulus it is a block headed as Tidal heads a stream, `v3 $`:
-- |
-- |     v3 $ ch3 "<[c4,e4,g4] [a3,c4,e4]>" "0 1 2 3" # arpup 4
-- |
-- | Evaluating the block writes the card; `vetula $ …` makes a new one (the
-- | page names it with the smallest free number). Limulus subscribes to the
-- | stage on the same socket it sends blocks on, so these frames arrive among
-- | the replies and must be told apart from them.
module Limulus.Stage
  ( CardLine
  , cardLine
  , cardKey
  , StageFrame(..)
  , readFrame
  , isStageFrame
  , freeCard
  , bodyOf
  , cardBlock
  ) where

import Prelude

import Data.Array (any, filter, find, mapMaybe, range)
import Data.Array as Array
import Data.Either (hush)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Nullable (Nullable, toMaybe)
import Data.String (Pattern(..), indexOf, joinWith, split, stripPrefix, trim)
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Simple.JSON (readJSON)

-- | A card block: which card (`Nothing` for `vetula $`, a new one) and its line.
type CardLine = { card :: Maybe Int, body :: String }

-- | `v3 $ …` or `vetula $ …`; the body's lines joined into one, as a card is
-- | one line (its `# layer`s may be written one per line).
cardLine :: String -> Maybe CardLine
cardLine block = do
  at <- indexOf (Pattern "$") block
  let
    head = trim (CU.take at block)
    body = bodyOf block
  card <- case head of
    "vetula" -> Just Nothing
    _ -> Just <$> (stripPrefix (Pattern "v") head >>= Int.fromString)
  pure { card, body }

-- | What follows the `$`, one line, spaces trimmed.
bodyOf :: String -> String
bodyOf block = case indexOf (Pattern "$") block of
  Nothing -> trim block
  Just at -> joinWith " " (filter (_ /= "") (map trim (split (Pattern "\n") (CU.drop (at + 1) block))))

cardKey :: Int -> String
cardKey n = "vetula/v" <> show n

cardOfKey :: String -> Maybe Int
cardOfKey key = stripPrefix (Pattern "vetula/v") key >>= Int.fromString

data StageFrame
  = Table (Map Int String)
  | Written Int (Maybe String)
  | Open Int
  | Rejected Int String

isStageFrame :: String -> Boolean
isStageFrame msg = any (\p -> isJust (stripPrefix (Pattern p) msg))
  [ "stage-texts ", "stage-text ", "stage-open ", "stage-reject " ]

-- | A stage frame about a Vetula card, or `Nothing` (another slot's object, or
-- | not a stage frame at all).
readFrame :: String -> Maybe StageFrame
readFrame msg =
  (stripPrefix (Pattern "stage-texts ") msg >>= \json -> do
      table :: Object { text :: String } <- hush (readJSON json)
      pure (Table (Map.fromFoldable (mapMaybe entry (Object.toUnfoldable table)))))
  `orElse`
  (stripPrefix (Pattern "stage-text ") msg >>= \json -> do
      w :: { key :: String, text :: Nullable String } <- hush (readJSON json)
      n <- cardOfKey w.key
      pure (Written n (toMaybe w.text)))
  `orElse`
  (stripPrefix (Pattern "stage-open ") msg >>= \json -> do
      w :: { key :: String } <- hush (readJSON json)
      Open <$> cardOfKey w.key)
  `orElse`
  (stripPrefix (Pattern "stage-reject ") msg >>= \json -> do
      w :: { key :: String, reason :: String } <- hush (readJSON json)
      n <- cardOfKey w.key
      pure (Rejected n w.reason))
  where
  entry (Tuple key v) = (\n -> Tuple n v.text) <$> cardOfKey key
  orElse a b = case a of
    Just _ -> a
    Nothing -> b

-- | A card's line as a block, broken where the card breaks it: the sequence
-- | on the head line, then each `# layer` (and each layer's gate) on its own,
-- | indented as a Tidal continuation. `#` inside quotes is not a break.
-- | (`bodyOf` joins the lines again, so the stage still holds one line.)
cardBlock :: Int -> String -> String
cardBlock n line = "v" <> show n <> " $ " <> joinWith "\n  # " (splitHashes line)

splitHashes :: String -> Array String
splitHashes line = map trim (go [] "" false (CU.toCharArray line))
  where
  go acc cur quoted cs = case Array.uncons cs of
    Nothing -> acc <> [ cur ]
    Just { head: c, tail }
      | c == '"' -> go acc (cur <> CU.singleton c) (not quoted) tail
      | c == '#' && not quoted -> go (acc <> [ cur ]) "" quoted tail
      | otherwise -> go acc (cur <> CU.singleton c) quoted tail

-- | The smallest card number neither on the stage nor heading a block.
freeCard :: Map Int String -> (Int -> Boolean) -> Int
freeCard cards inBuffer =
  let n = Map.size cards + 2
  in fromMaybe n (find (\k -> not (Map.member k cards) && not (inBuffer k)) (range 1 n))
