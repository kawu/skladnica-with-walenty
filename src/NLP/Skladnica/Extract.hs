{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


-- | A module responsible for identifying occurrences of the
-- Walenty valency dictionary entries in the Skladnica treebank.


module NLP.Skladnica.Extract
( extractGrammar
) where


import           Control.Monad                 (forM_, when)

-- import qualified Data.ByteString               as BS
import           Data.Either                   (lefts)
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (mapMaybe)
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Tree                     as R

import qualified System.FilePath.Find          as F

import qualified NLP.Skladnica                 as Skl
import qualified NLP.Walenty                   as W

import qualified NLP.Skladnica.Walenty.Grammar as G
import qualified NLP.Skladnica.Walenty.MweTree as MWE


------------------------------------------------------------------------------
-- Grammar Extraction Tests
--------------------------------------------------------------------------------


data Extract = Extract
  { gramSet     :: S.Set G.ET
  -- ^ The resulting grammar
  , parsedFiles :: S.Set FilePath
  -- ^ Names of the parsed files (i.e., for which at least one
  -- parsed tree has been extracted)
  , seenFiles   :: S.Set FilePath
  -- ^ Names of the seen files
  , mweFiles    :: S.Set FilePath
  -- ^ Names of the files in which WMEs have been found
  , freqMap     :: M.Map T.Text Int
  -- ^ Frequency map extracted from the treebank
  } deriving (Show, Eq, Ord)


emptyExtract :: Extract
emptyExtract = Extract S.empty S.empty S.empty S.empty M.empty


showExtract :: Extract -> IO ()
showExtract Extract{..} = do
  putStr "PARSED FILES: " >> print (S.size parsedFiles)
  putStr "SEEN FILES: " >> print (S.size seenFiles)
  putStr "MWE FILES: " >> print (S.size mweFiles)
  putStr "GRAM TREES: " >> print (S.size gramSet)
  putStr "FREQ MAP: " >> print (M.size freqMap)


-- | Read all verb entries from Walenty and search for them
-- in Skladnica treebank.
-- Extract the grammar from the resulting syntactic trees
-- (both with and without MWEs).
extractGrammar
  :: FilePath -- ^ Skladnica XML file
  -- -> IO Extract
  -> IO ()
extractGrammar skladnicaXML = do
  trees <- MWE.readTop skladnicaXML
  -- flip E.execStateT emptyExtract $ do
  forM_ trees $ \sklTree0 -> do
      let mweTree = fmap MWE.sklNode (MWE.emboss sklTree0)
          sklTree = fmap MWE.sklNode sklTree0
          sklETs  = G.extractGrammar sklTree
          mweETs = G.extractGrammar mweTree
          est = sklETs `S.union` mweETs
          realMweETs = mweETs `S.difference` sklETs
      when (S.null est) $ do
        putStrLn "WARNING: something went wrong..."
      when (not $ S.null realMweETs) $ do
        -- putStrLn "MWE elementary trees found:\n"
        let trees = map (fmap show) (S.toList realMweETs)
        putStrLn $ R.drawForest trees
