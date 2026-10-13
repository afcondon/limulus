-- | What a thing dragged from a machine's drawer means where it lands in
-- | Limulus. One rule per kind, on one principle (AC, 2026-10-13, from
-- | Vetula's): dropped ONTO a block of its machine it changes that block, the
-- | way that block naturally takes another of its kind; dropped anywhere
-- | else it starts a new block with a good default. Elsewhere inside some
-- | other block, it is its plain text where it fell.
-- |
-- |   a Vetula progression  onto a voice: the voice plays it instead
-- |                         elsewhere: a new voice playing it
-- |   a Balistes beat       onto a `balistes $` line: joins its alternation
-- |   an Odonus patch       onto an `odonus $ "<…>"` line: joins it
-- |                         elsewhere: a new line playing it
-- |   a Selene module       onto a `selene $` line: replaces what its bank
-- |                         plays (as a drop on that bank on the page)
-- |                         elsewhere: a new line on a free bank that takes it
-- |
-- | Onto a line of its machine that has no pattern of names to join (a
-- | `balistes $ hush`, an `odonus $ rotate 2`), the new line goes after it.
-- |
-- | The block is rewritten, never sent: evaluating it stays the composer's.
module Limulus.Drop
  ( Outcome(..)
  , kinds
  , outcome
  , wordLine
  , joinWord
  ) where

import Prelude

import Data.Array (filter, index)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), indexOf, lastIndexOf, split, trim)
import Data.String as Str
import Data.String.CodeUnits as CU
import Limulus.Stage as Stage

-- | The kinds a drop can carry (as `application/x-<kind>`), most specific
-- | first.
kinds :: Array String
kinds = [ "vetula-progression", "balistes-beat", "odonus-patch", "selene-module" ]

data Outcome
  = NewBlock String   -- a block of its own, where it fell
  | Rewrite String    -- the block it fell on, rewritten
  | After String      -- a block of its own, after the one it fell on (a block
                      -- of its machine that has no way to take it)
  | Bare String       -- its plain text, where it fell

-- | What dropping `payload` of `kind` means: onto the block `onto` (its
-- | text), or onto no block. `freeVoice` is the head of a voice no block
-- | uses yet (Vetula's new voice).
outcome :: { freeVoice :: String } -> String -> String -> Maybe String -> Outcome
outcome ctx kind payload onto = case kind of
  "vetula-progression" -> case onto of
    Nothing -> NewBlock (ctx.freeVoice <> " $ vetula \"" <> payload <> "\"")
    Just text -> maybe (Bare ("\"" <> payload <> "\"")) Rewrite (repoint payload text)
  "balistes-beat" -> word "balistes"
  "odonus-patch" -> word "odonus"
  "selene-module" -> selene (fields payload) onto
  _ -> Bare payload
  where
  word machine = case onto of
    Nothing -> NewBlock (wordLine machine payload)
    Just text -> case joinWord machine payload text of
      Just joined -> Rewrite joined
      Nothing | isLineOf machine text -> After (wordLine machine payload)
              | otherwise -> Bare payload

-- | Whether a block is a `<machine> $ …` line.
isLineOf :: String -> String -> Boolean
isLineOf machine text = case CU.stripPrefix (Pattern machine) (trim text) of
  Just rest -> CU.take 1 (trim rest) == "$"
  Nothing -> false

-- | A Selene module (`module`, `bank`, `suffix`, as the page's drag carries
-- | them): onto a `selene $` line, the module on that line's bank, as a drop
-- | on that bank on the page would be; elsewhere, a new line on the bank the
-- | page suggests. `off` frees a line's bank.
selene :: Array String -> Maybe String -> Outcome
selene f onto = case onto >>= bankOfSelene of
  Just bank -> Rewrite (seleneLine bank)
  Nothing | Just text <- onto, isLineOf "selene" text -> After suggested
          | bankHere == "" || moduleHere == "off" -> Bare moduleHere
          | otherwise -> case onto of
              Nothing -> NewBlock suggested
              Just _ -> Bare moduleHere
  where
  moduleHere = fromMaybe "" (index f 0)
  bankHere = fromMaybe "" (index f 1)
  suffix = fromMaybe "" (index f 2)
  suggested = seleneLine bankHere
  seleneLine bank
    | moduleHere == "off" = "selene $ off " <> bank
    | otherwise = case indexOf (Pattern " ") moduleHere of
        Just at -> "selene $ " <> Str.take at moduleHere <> " " <> bank <> Str.drop at moduleHere <> suffix
        Nothing -> "selene $ " <> moduleHere <> " " <> bank <> suffix

-- | The bank a `selene $ <kind> <bank> …` (or `selene $ off <bank>`) line
-- | addresses.
bankOfSelene :: String -> Maybe String
bankOfSelene text = do
  rest <- CU.stripPrefix (Pattern "selene") (trim text)
  body <- CU.stripPrefix (Pattern "$") (trim rest)
  index (filter (_ /= "") (split (Pattern " ") (trim body))) 1

fields :: String -> Array String
fields = split (Pattern "\t")

-- | A Vetula voice's block with `name` as its progression, its sequence and
-- | manner kept; Nothing for a block that is not a voice.
repoint :: String -> String -> Maybe String
repoint name text = do
  sl <- Stage.stageLine text
  at <- indexOf (Pattern "$") text
  body <- Stage.repoint name sl.body
  pure case sl.obj of
    Just obj -> Stage.blockOf obj body
    Nothing -> Str.take (at + 1) text <> " " <> body

-- | A new line playing one saved thing by its word.
wordLine :: String -> String -> String
wordLine machine w = machine <> " $ \"<" <> w <> ">\""

-- | The block with `w` added to its pattern, if the block is a
-- | `<machine> $ "…"` line: before the `>` of an alternation (`<a b>` →
-- | `<a b w>`), else around what is there (`a` → `<a w>`, `a b` →
-- | `<[a b] w>`, so a sequence stays one step of the alternation). Nothing for
-- | any other block.
joinWord :: String -> String -> String -> Maybe String
joinWord machine w block = do
  let t = trim block
  rest <- CU.stripPrefix (Pattern machine) t
  dollar <- indexOf (Pattern "$") rest
  open <- indexOf (Pattern "\"") rest
  close <- lastIndexOf (Pattern "\"") rest
  if dollar > open || close <= open || trim (CU.slice 0 dollar rest) /= "" then Nothing else do
    let
      inner = trim (CU.slice (open + 1) close rest)
      joined
        | inner == "" = "<" <> w <> ">"
        | CU.take 1 inner == "<" && CU.takeRight 1 inner == ">" =
            trim (CU.dropRight 1 inner) <> " " <> w <> ">"
        | single inner = "<" <> inner <> " " <> w <> ">"
        | otherwise = "<[" <> inner <> "] " <> w <> ">"
    pure (machine <> CU.take (open + 1) rest <> joined <> CU.drop close rest)
  where
  single s = indexOf (Pattern " ") s == Nothing && indexOf (Pattern "[") s == Nothing
    && indexOf (Pattern "{") s == Nothing && indexOf (Pattern "(") s == Nothing
