{-# LANGUAGE OverloadedStrings #-}


-- | A module responsible for parsing the SEJF dictionary
-- (http://zil.ipipan.waw.pl/SEJF).


module NLP.Skladnica.Walenty.Sejf
( readSejf
, parseSejf
, parseEntry
) where

import           Control.Arrow     (first)
import           Control.Monad     (guard, msum)

import           Data.Text         (Text)
import qualified Data.Text         as T
import qualified Data.Text.Lazy    as L
import qualified Data.Text.Lazy.IO as L



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


-- -- | Split the entry on the first comma separator.
-- -- Take into account potential escaped commas.
-- splitEntry :: Char -> String -> (String, String)
-- splitEntry sep =
--   go
--   where
--     go xs = case xs of
--       '\\' : spec : ys -> first (spec :) (go ys)
--       x : ys -> if x == sep
--         then ("", ys)
--         else first (x:) (go ys)
--       [] -> error "splitEntry.go: no comma separator?"

