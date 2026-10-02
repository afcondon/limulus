-- | **Machines' lines on the stage.** Some of what the machines hold is one
-- | line of text, kept as an object on the rig's stage, which the machine's page
-- | and Limulus both read and write (docs/kb/plans/text-on-the-stage.md). In
-- | Limulus each is a block headed as Tidal heads a stream:
-- |
-- |     v3 $ ch3 "<[c4,e4,g4] [a3,c4,e4]>" "0 1 2 3" # arpup 4
-- |     conspicillum $ s "fd-beat-bar" # walk 0.2 0.1 0.1 # grid 8
-- |
-- | - a **Vetula card** (`vetula/v3`): `v3 $` writes card 3, `vetula $ ch…`
-- |   makes a new one, named with the smallest free number;
-- | - **Conspicillum's cloud** (`conspicillum/line`): the scene's line in
-- |   `Reef.Conspicillum.Notation`, applied by the page as if typed in its own
-- |   line box.
-- |
-- | Evaluating the block writes the object. Limulus subscribes to the
-- | stage on the same socket it sends blocks on, so these frames arrive among
-- | the replies and must be told apart from them.
module Limulus.Stage
  ( Obj(..)
  , objKey
  , headOf
  , ownerOf
  , StageLine
  , stageLine
  , StageFrame(..)
  , readFrame
  , isStageFrame
  , freeCard
  , bodyOf
  , blockOf
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

-- | An object on the stage that Limulus edits as a block.
data Obj = Card Int | Cloud

derive instance Eq Obj
derive instance Ord Obj

objKey :: Obj -> String
objKey = case _ of
  Card n -> "vetula/v" <> show n
  Cloud -> "conspicillum/line"

objOfKey :: String -> Maybe Obj
objOfKey key
  | key == "conspicillum/line" = Just Cloud
  | otherwise = Card <$> (stripPrefix (Pattern "vetula/v") key >>= Int.fromString)

-- | The block's head word: `v3`, `conspicillum`.
headOf :: Obj -> String
headOf = case _ of
  Card n -> "v" <> show n
  Cloud -> "conspicillum"

-- | The page that owns the object, for the log.
ownerOf :: Obj -> String
ownerOf = case _ of
  Card _ -> "Vetula"
  Cloud -> "Conspicillum"

-- | A stage block: which object (`Nothing` for `vetula $ ch…`, a new card)
-- | and its line.
type StageLine = { obj :: Maybe Obj, body :: String }

-- | `v3 $ …`, `vetula $ ch…` or `conspicillum $ …`; the body's lines joined
-- | into one, as the object is one line (its `# …` terms may be written one
-- | per line).
stageLine :: String -> Maybe StageLine
stageLine block = do
  at <- indexOf (Pattern "$") block
  let
    head = trim (CU.take at block)
    body = bodyOf block
  obj <- case head of
    -- `vetula $ ch…` is a new card; any other `vetula $` line (mark, loop) is
    -- a cue for the rig
    "vetula" | isJust (stripPrefix (Pattern "ch") body) -> Just Nothing
    "vetula" -> Nothing
    "conspicillum" -> Just (Just Cloud)
    _ -> Just <$> (Card <$> (stripPrefix (Pattern "v") head >>= Int.fromString))
  pure { obj, body }

-- | What follows the `$`, one line, spaces trimmed.
bodyOf :: String -> String
bodyOf block = case indexOf (Pattern "$") block of
  Nothing -> trim block
  Just at -> joinWith " " (filter (_ /= "") (map trim (split (Pattern "\n") (CU.drop (at + 1) block))))

data StageFrame
  = Table (Map Obj String)
  | Written Obj (Maybe String)
  | Open Obj
  | Rejected Obj String

isStageFrame :: String -> Boolean
isStageFrame msg = any (\p -> isJust (stripPrefix (Pattern p) msg))
  [ "stage-texts ", "stage-text ", "stage-open ", "stage-reject " ]

-- | A stage frame about an object Limulus edits, or `Nothing` (another slot's object, or
-- | not a stage frame at all).
readFrame :: String -> Maybe StageFrame
readFrame msg =
  (stripPrefix (Pattern "stage-texts ") msg >>= \json -> do
      table :: Object { text :: String } <- hush (readJSON json)
      pure (Table (Map.fromFoldable (mapMaybe entry (Object.toUnfoldable table)))))
  `orElse`
  (stripPrefix (Pattern "stage-text ") msg >>= \json -> do
      w :: { key :: String, text :: Nullable String } <- hush (readJSON json)
      o <- objOfKey w.key
      pure (Written o (toMaybe w.text)))
  `orElse`
  (stripPrefix (Pattern "stage-open ") msg >>= \json -> do
      w :: { key :: String } <- hush (readJSON json)
      Open <$> objOfKey w.key)
  `orElse`
  (stripPrefix (Pattern "stage-reject ") msg >>= \json -> do
      w :: { key :: String, reason :: String } <- hush (readJSON json)
      o <- objOfKey w.key
      pure (Rejected o w.reason))
  where
  entry (Tuple key v) = (\o -> Tuple o v.text) <$> objOfKey key
  orElse a b = case a of
    Just _ -> a
    Nothing -> b

-- | An object's line as a block, broken where a card breaks it: the head
-- | term on the first line, then each `# term` on its own, indented as a Tidal
-- | continuation. `#` inside quotes is not a break. (`bodyOf` joins the lines
-- | again, so the stage still holds one line.)
blockOf :: Obj -> String -> String
blockOf obj line = headOf obj <> " $ " <> joinWith "\n  # " (splitHashes line)

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
freeCard :: Map Obj String -> (Int -> Boolean) -> Int
freeCard objects inBuffer =
  let n = Map.size objects + 2
  in fromMaybe n (find (\k -> not (Map.member (Card k) objects) && not (inBuffer k)) (range 1 n))
