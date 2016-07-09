{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


-- | A module responsible for identifying occurrences of the
-- Walenty valency dictionary entries in the Skladnica treebank.


module NLP.Skladnica.Extract
( extractGrammar
, testParser
) where


import           Control.Monad                 (forM_, when)

-- import qualified Data.ByteString               as BS
import           Data.Either                   (lefts)
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (mapMaybe)
import qualified Data.MemoCombinators          as Memo
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Tree                     as R

import qualified System.FilePath.Find          as F

import qualified NLP.Partage.AStar             as AStar
import qualified NLP.Partage.AStar.Deriv       as Deriv
import qualified NLP.Partage.AStar.Deriv.Gorn  as Gorn
import qualified NLP.Partage.AStar.DepTree     as Dep
import qualified NLP.Partage.DAG               as DAG

import qualified NLP.Skladnica                 as Skl
import qualified NLP.Walenty                   as W

import qualified NLP.Skladnica.Walenty.Grammar as G
import qualified NLP.Skladnica.Walenty.MweTree as MWE
import qualified NLP.Skladnica.Walenty.Search  as Search


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
        putStrLn
          . R.drawForest . map (fmap show)
          . S.toList $ realMweETs


-- | Extract local grammars from the individual sentences in the input forest,
-- parse with the local grammars and show the corresponding derivation and
-- dependency trees.
testParser
  :: FilePath -- ^ Skladnica XML file
  -> String   -- ^ Start symbol
  -> IO ()
testParser skladnicaXML begSym0 = do
  trees <- MWE.readTop skladnicaXML
  forM_ trees $ \sklTree0 -> do
      let mweTree = fmap MWE.sklNode (MWE.emboss sklTree0)
          sklTree = fmap MWE.sklNode sklTree0
          sklETs  = G.extractGrammar sklTree
          mweETs = G.extractGrammar mweTree
          est = sklETs `S.union` mweETs
      when (S.null est) $ do
        putStrLn "WARNING: something went wrong..."
      let begSym = T.pack begSym0
          gram = buildGram est
          termMemo = Memo.wrap read show $ Memo.list Memo.char
          auto = AStar.mkAuto termMemo gram
          sent = baseForms sklTree
          sentLen = length sent
          input = AStar.fromList sent
      hype <- AStar.earleyAuto auto input
      let derivList = Deriv.derivTrees hype begSym sentLen
      T.putStrLn $ "# PARSING: " `T.append` T.unwords (orthForms sklTree)
      putStrLn $ "# NUMBER OF DERIVS: " ++ show (length derivList)
      forM_ derivList $ \derivTree -> do
        let depTree = Dep.fromDeriv . Gorn.fromDeriv $ derivTree
        putStrLn ""
        putStrLn . R.drawTree . fmap show . Deriv.deriv4show $ derivTree
        putStrLn ""
        putStrLn . R.drawTree . fmap show $ depTree


------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------


-- | Build a grammar from the given set of ETs.
buildGram :: S.Set G.ET -> DAG.Gram T.Text T.Text
buildGram = DAG.mkGram . map (,1) . S.toList


-- | Retrieve terminal base forms from the given syntactic tree.
baseForms :: Search.SklTree -> [T.Text]
baseForms = map Skl.base . Search.terminals


-- | Retrieve terminal orth forms from the given syntactic tree.
orthForms :: Search.SklTree -> [T.Text]
orthForms = map Skl.orth . Search.terminals
