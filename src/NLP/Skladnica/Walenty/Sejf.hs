{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


-- | A module responsible for parsing the SEJF dictionary
-- (http://zil.ipipan.waw.pl/SEJF).


module NLP.Skladnica.Walenty.Sejf
( SejfEntry(..)
, CaseSensitivity (..)
, partition
, querify
, querifyOrth
, querify'
, querifyOrth'
-- * Parsing
, readSejf
, parseSejf
, parseEntry
) where

import           Control.Arrow                 (first)
import           Control.Monad                 (guard, msum)

import qualified Data.Char                     as C
import           Data.List                     (isInfixOf)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as L
import qualified Data.Text.Lazy.IO             as L
import qualified Data.Tree                     as R

import qualified NLP.Skladnica                 as S
import qualified NLP.Skladnica.Walenty.Search  as E
import qualified NLP.Skladnica.Walenty.Search2 as E2
import qualified NLP.Skladnica.Walenty.Mapping as Map

-- import Debug.Trace (trace)


--------------------------------------------------------------------------------
-- Sejf Entry
--------------------------------------------------------------------------------


-- | An entry of the SEJF dictionary in the extensional format.
data SejfEntry = SejfEntry
  { orth :: Text
    -- ^ Orthographic, inflected form
  , base :: Text
    -- ^ Base form of the entry
  , tag  :: Text
    -- ^ Morphosyntactic tag
  } deriving (Show, Eq, Ord)


--------------------------------------------------------------------------------
-- Entry Convertion
--------------------------------------------------------------------------------


-- | Should the comparison be case sensitive or should the case be ignored?
data CaseSensitivity
  = CaseSensitive
  | IgnoreCase


-- | Check if the head terminal has the corresponding tag
-- and if every part of the `orth` form is present in terminal
-- leaves.
querify :: CaseSensitivity -> SejfEntry -> E.Expr E.SklTree
querify caseSens = querifyOrth caseSens . orth


-- | Generalized querify based on the orthographic form only.
querifyOrth :: CaseSensitivity -> Text -> E.Expr E.SklTree
querifyOrth caseSens orth = E.andQ
  [ checkTrunk
  , E.IfThenElse checkLeaves markLeaves (E.B False)
  ]
  where
    -- below, it is easier to use `Satisfy` to check that terminals
    -- occur in appropriate order, but finally the leaves have to be
    -- marked as MWE components separately (`Satisfy` doesn't mark),
    -- hense the `markLeaves` function.
    checkLeaves = E.Satisfy $ \t ->
      casedOrthParts
      `isInfixOf`
      map withCase (leaves t)
      where
        leaves = map S.orth . E.terminals
    -- check that at leat one of the MWE componenets is on the trunk.
    checkTrunk = E.trunk . E.hasOrth $ \word ->
      withCase word `elem` casedOrthParts
    -- TODO: note that `markLeaves` can mark more than identified
    -- with `checkLeaves`!
    markLeaves = E.andQ
      [ E.ancestor . E.hasOrth $ \word ->
          withCase word == casedForm
      | casedForm <- casedOrthParts ]
    -- take into account case sensitivity
    withCase = case caseSens of
      CaseSensitive -> id
      IgnoreCase -> T.toLower
    casedOrthParts = map withCase (partition orth)


--------------------------------------------------------------------------------
-- Entry Convertion 2
--------------------------------------------------------------------------------


-- | Check if the head terminal has the corresponding tag
-- and if every part of the `orth` form is present in terminal
-- leaves.
querify' :: CaseSensitivity -> SejfEntry -> E2.Expr Map.Edge 'E2.Tree
querify' caseSens = querifyOrth' caseSens . orth


-- | Generalized querify based on the orthographic form only.
querifyOrth' :: CaseSensitivity -> Text -> E2.Expr Map.Edge 'E2.Tree
querifyOrth' caseSens orth = E2.andQ
  [ checkTrunk
  , E2.ifThenElse checkLeaves markLeaves (E2.B False)
  ]
  where
    -- below, it is easier to use `Satisfy` to check that terminals
    -- occur in appropriate order, but finally the leaves have to be
    -- marked as MWE components separately (`Satisfy` doesn't mark),
    -- hense the `markLeaves` function.
    checkLeaves = E2.SatisfyTree $ \t ->
      casedOrthParts
      `isInfixOf`
      map withCase (leaves t)
      where
        leaves = map S.orth . Map.terminals
    -- check that at leat one of the MWE componenets is on the trunk.
    checkTrunk = Map.trunk . Map.hasOrth $ \word ->
      withCase word `elem` casedOrthParts
    -- TODO: note that `markLeaves` can mark more than identified
    -- with `checkLeaves`!
    markLeaves = E2.andQ
      [ E2.anyAncestor . Map.hasOrth $ \word ->
          withCase word == casedForm
      | casedForm <- casedOrthParts ]
    -- take into account case sensitivity
    withCase = case caseSens of
      CaseSensitive -> id
      IgnoreCase -> T.toLower
    casedOrthParts = map withCase (partition orth)


--------------------------------------------------------------------------------
-- Partition
--------------------------------------------------------------------------------


-- | Extract parts of the given textual form, using spaces and interpunction
-- characters as separators, the latter being left in the resulting list.
partition :: Text -> [Text]
partition =
  filter (not . T.null) . go
  where
    go text
      | T.null right = [left]
      | headSat C.isSpace right =
          left : go (T.tail right)
      | headSat C.isPunctuation right =
          left : T.singleton (T.head right) : go (T.tail right)
      | otherwise = error "go: ???"
      where
        (left, right) = T.break (\c -> C.isSpace c || C.isPunctuation c) text
    headSat p t = case T.uncons t of
      Just (x, _) -> p x
      Nothing -> False


--------------------------------------------------------------------------------
-- Entry Parser
--------------------------------------------------------------------------------


-- | Read and parse the SEJF dictionary from a file.
readSejf :: FilePath -> IO [SejfEntry]
readSejf = fmap parseSejf . L.readFile


-- | Parse the entire SEJF dictionary, line by line.
parseSejf :: L.Text -> [SejfEntry]
parseSejf = map (parseEntry . L.toStrict) . L.lines


-- | Parse a SEJF entry line.
parseEntry :: Text -> SejfEntry
parseEntry entry = takeJust $ do
  (orth', baseTag) <- splitWith ',' '\\' entry
  (base', tag') <- splitWith ':' '\\' baseTag
  return SejfEntry
    { orth = orth'
    , base = base'
    , tag  = tag' }
  where
    takeJust (Just x) = x
    takeJust Nothing = error "splitWith: no separator?"


-- | Split the entry on the first separator.
-- Take into account potential escaped commas.
splitWith
  :: Char  -- ^ Separator character
  -> Char  -- ^ Espape character
  -> Text  -- ^ Text to split
  -> Maybe (Text, Text)
splitWith sep esc =
  go
  where
    go t = msum . map ($t) $
      [escape, separator, other]
    escape t0 = do
      (escapeChar, t1) <- T.uncons t0
      guard $ escapeChar == esc
      (specialChar, t2) <- T.uncons t1
      first (T.cons specialChar) <$> go t2
    separator t0 = do
      (sepChar, t) <- T.uncons t0
      guard $ sepChar == sep
      return ("", t)
    other t0 = do
      (char, t) <- T.uncons t0
      first (T.cons char) <$> go t
