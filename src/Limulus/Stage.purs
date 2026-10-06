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
  , tableProgressions
  , cardProgression
  , voiceLetter
  , voiceOfName
  , routeVoice
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
import Data.String (Pattern(..), indexOf, joinWith, split, stripPrefix, stripSuffix, trim)
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

-- | The block's head word: `Q` (a Vetula voice), `conspicillum`.
headOf :: Obj -> String
headOf = case _ of
  Card n -> voiceLetter n
  Cloud -> "conspicillum"

-- | A Vetula voice's name: P..W (AC, 2026-10-06), so a number is only ever
-- | a MIDI channel. Inside (the stage key) a voice is numbered, P = 1; as
-- | `Reef.Vetula.VoiceName`, which Limulus does not import.
voiceLetters :: Array String
voiceLetters = [ "P", "Q", "R", "S", "T", "U", "V", "W" ]

voiceLetter :: Int -> String
voiceLetter n = fromMaybe ("v" <> show n) (Array.index voiceLetters (n - 1))

-- | A voice from its letter, or the old `v3`.
voiceOfName :: String -> Maybe Int
voiceOfName s = case Array.elemIndex s voiceLetters of
  Just i -> Just (i + 1)
  Nothing -> stripPrefix (Pattern "v") s >>= Int.fromString

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
    -- `conspicillum $ hush` is for the rig, not a line
    "conspicillum" | body == "hush" -> Nothing
    "conspicillum" -> Just (Just Cloud)
    _ -> Just <$> (Card <$> voiceOfName head)
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
  -- | A progression saved (`Just` its chords) or deleted in Vetula: what a
  -- | voice line (`v1 $ vetula "name"`) plays.
  | Progression String (Maybe String)
  -- | A block of text a page hands over to add to the buffer (a mark, as
  -- | code), and whose it is (`odonus/mark`).
  | Paste String String

isStageFrame :: String -> Boolean
isStageFrame msg = any (\p -> isJust (stripPrefix (Pattern p) msg))
  [ "stage-texts ", "stage-text ", "stage-open ", "stage-reject ", "stage-paste " ]

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
  (stripPrefix (Pattern "stage-text ") msg >>= \json -> do
      w :: { key :: String, text :: Nullable String } <- hush (readJSON json)
      name <- stripPrefix (Pattern progPrefix) w.key
      pure (Progression name (toMaybe w.text)))
  `orElse`
  (stripPrefix (Pattern "stage-open ") msg >>= \json -> do
      w :: { key :: String } <- hush (readJSON json)
      Open <$> objOfKey w.key)
  `orElse`
  (stripPrefix (Pattern "stage-paste ") msg >>= \json -> do
      w :: { key :: String, text :: String } <- hush (readJSON json)
      pure (Paste w.key w.text))
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

progPrefix :: String
progPrefix = "vetula/progression/"

-- | The names of the progressions saved in Vetula, from the whole table
-- | (`Nothing` for any other frame).
tableProgressions :: String -> Maybe (Array String)
tableProgressions msg = do
  json <- stripPrefix (Pattern "stage-texts ") msg
  table :: Object { text :: String } <- hush (readJSON json)
  pure (mapMaybe (stripPrefix (Pattern progPrefix)) (Object.keys table))

-- | The progression a card's line names, if it names one: `vetula "name" …`
-- | or `ch3 name "0 1" …` (rather than chords written in, or `-`).
cardProgression :: String -> Maybe String
cardProgression body = case filter (_ /= "") (split (Pattern " ") (trim body)) of
  toks | Array.head toks == Just "vetula" -> do
    q <- Array.index toks 1
    inner <- stripPrefix (Pattern "\"") q
    pure (fromMaybe inner (stripSuffix (Pattern "\"") inner))
  toks | isJust (Array.head toks >>= stripPrefix (Pattern "ch")) -> case Array.index toks 1 of
    -- a name, quoted or not (chords written in have brackets)
    Just t | t /= "-" && indexOf (Pattern "[") t == Nothing ->
      Just (fromMaybe t (stripPrefix (Pattern "\"") t >>= stripSuffix (Pattern "\"")))
    _ -> Nothing
  _ -> Nothing

-- | The voice a route line names, if it names one: `odonus.out <- vetula Q`.
routeVoice :: String -> Maybe String
routeVoice block = do
  at <- indexOf (Pattern "<-") block
  case filter (_ /= "") (split (Pattern " ") (trim (CU.drop (at + 2) block))) of
    [ "vetula", v ] | v /= "key" -> Just v
    _ -> Nothing
