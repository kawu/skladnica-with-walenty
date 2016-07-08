{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


-- | A module responsible for identifying occurrences of the
-- Walenty valency dictionary entries in the Skladnica treebank.


module NLP.Skladnica.Extract
( mapMWEs
) where


import           Control.Monad                 (forM_)

import           Data.Either                   (lefts)
import           Data.Maybe                    (mapMaybe)
import qualified Data.Set                      as S
import           Data.Tree                     as R
import qualified Data.Text.IO                  as T
import           Data.Text                     (Text)

import qualified System.FilePath.Find          as F

import qualified NLP.Skladnica                 as Skl
import qualified NLP.Walenty                   as W

import qualified NLP.Skladnica.Walenty.Prune   as P
import qualified NLP.Skladnica.Walenty.Search2 as Q
import qualified NLP.Skladnica.Walenty.Mapping as Mapping
-- import qualified NLP.Skladnica.Walenty.Select  as Select
import qualified NLP.Skladnica.Walenty.Sejf    as Sejf
import qualified NLP.Skladnica.Walenty.NcpNEs  as NE
import qualified NLP.Skladnica.Walenty.MweTree as MWE


------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------


-- | Recursively retrieve all Xml files from the given directory.
getXmlFiles :: FilePath -> IO [FilePath]
getXmlFiles dir = do
  F.find F.always (F.fileName F.~~? "*.xml") dir


------------------------------------------------------------------------------
-- Grammar Extraction Tests
--------------------------------------------------------------------------------


-- | Map individual MWEs on Składnica and print the resulting trees on stdin.
mapMWEs
  :: FilePath -- ^ Skladnica directory
  -> FilePath -- ^ Walenty file
  -> FilePath -- ^ Walenty expansion file
  -> FilePath -- ^ SEJF file
  -> FilePath -- ^ NCP directory (for NEs)
  -> IO ()
mapMWEs skladnicaDir walentyPath expansionPath sejfPath ncpPath = do
  -- read *lexicalized* verbal entries from Walenty
  walenty <- readWalenty walentyPath expansionPath
--   putStr "Number of lexical entries: " >> print (length walenty)
  -- read SEJF dictionary
  sejf0 <- Sejf.readSejf sejfPath
  let sejf = sejfPartition Sejf.orth sejf0
  -- read NCP-NEs dictionary
  nes0 <- nubOrd <$> NE.nesInCorpus ncpPath
  let nes = sejfPartition id nes0
--   putStrLn $ "===== NEs ===== "
--   forM_ nes $ \ne -> print ne
--   putStrLn $ "===== NEs END ===== "
  -- find all XML files
  xmlFiles <- getXmlFiles skladnicaDir
  -- per each XML file...
  forM_ xmlFiles (procPath walenty sejf nes)
  where
    -- procPath walenty sejf0 nes0 skladnicaXML = do
    procPath walenty sejf0 nes0 skladnicaXML = do
      -- putStrLn $ ">>> " ++ skladnicaXML ++ " <<<"
      putStr "<tree>"
      sklForest <- forestFromXml skladnicaXML
      let sentSet = case sklForest of
            sklTree : _ -> S.fromList $
              map Skl.orth (Mapping.terminals sklTree)
            _ -> S.empty
          sejf =
            [ entry
            | (entry, wordSet) <- sejf0
            , wordSet `S.isSubsetOf` sentSet ]
          nes =
            [ entry
            | (entry, wordSet) <- nes0
            , wordSet `S.isSubsetOf` sentSet ]
      let exprs1 = map Mapping.querify walenty
          exprs2 = map (Sejf.querify' Sejf.IgnoreCase) sejf
          exprs3 = map (Sejf.querifyOrth' Sejf.CaseSensitive) nes
--       E.lift $ putStrLn $ "Size of the sentSet: " ++ show (S.size sentSet)
--       E.lift $ putStrLn $ "Relevant SEJF expressions: " ++ show sejf
--       E.lift $ putStrLn $ "Relevant NES: " ++ show nes
      forM_ sklForest $ \sklTree -> do
        let mweTree = Mapping.markSklTree (exprs1 ++ exprs2 ++ exprs3) sklTree
        T.putStr . MWE.renderXml . MWE.mweTreeXml . MWE.fromOut $ mweTree
      putStrLn "</tree>"
--             label edge = case Skl.label (Skl.nodeLabel edge) of
--               Left nt -> (nid, Skl.cat nt)
--               Right t -> (nid, Skl.orth t)
--               where nid = Skl.nid (Skl.nodeLabel edge)
--             mwe idSet
--               | S.null idSet = ""
--               | otherwise = "MWE: " ++ show idSet
--             showNode (edge, idSet) = show (label edge, mwe idSet)
--         putStrLn . R.drawTree . fmap showNode $ mweTree


------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------


-- readWalenty
--   :: FilePath -- ^ Walenty filePath
--   -> FilePath -- ^ Expansion file
--   -> IO ()
readWalenty walentyPath expansionPath = do
  expMap <- W.readExpMap expansionPath
  walenty <-
       S.toList . S.fromList
     . mapMaybe P.pruneVerb
     -- NEW 01/06/2016: additional pruning is needed to remove non-lexical
     -- constraints introduced during in the process of expansion
     . map (W.expandVerb expMap)
     -- the first pruning is needed before expansions so that unsignificant
     -- expansions are not treated as lexical constraints
     . mapMaybe P.pruneVerb
     . lefts
    <$> W.readWalenty walentyPath
  return walenty


-- | Extract skladnica trees from the given XML file.
forestFromXml :: FilePath -> IO [Skl.Tree Skl.Node Skl.IsHead]
forestFromXml xml = do
  nodesDAG <- Skl.mkDAG <$> Skl.readTop xml
  return $ Skl.forest Skl.chosen 0 nodesDAG


-- | Use `Sejf.partition` to determine component words and put them (under the
-- form of a set) on the second position of the corresponding list elements.
-- Preserve only these entries which have more than one component words.
sejfPartition :: (a -> Text) -> [a] -> [(a, S.Set Text)]
-- sejfPartition f = map $ \x -> (x, S.fromList . Sejf.partition . f $ x)
sejfPartition f xs =
  [ (x, wordSet)
  | x <- xs
  , let wordSet = S.fromList . Sejf.partition . f $ x
  , S.size wordSet > 1 ]


--------------------------------------------------
-- Misc
--------------------------------------------------


nubOrd :: (Ord a) => [a] -> [a]
nubOrd = S.toList . S.fromList
