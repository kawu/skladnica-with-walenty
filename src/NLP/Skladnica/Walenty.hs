{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


-- | A module responsible for identifying occurrences of the
-- Walenty valency dictionary entries in the Skladnica treebank.


module NLP.Skladnica.Walenty
( runTest
) where


import           Control.Monad               (forM_)
import qualified Control.Monad.Reader        as E

import           Data.Either                 (lefts)
import           Data.Maybe                  (mapMaybe)
import qualified Data.Set                    as S
import qualified Data.Text.Lazy              as L
-- import           Data.Tree                   as R

import qualified System.FilePath.Find        as F

import qualified NLP.Skladnica               as S
import qualified NLP.Walenty                 as W

import qualified NLP.Skladnica.Walenty.Prune as P
import qualified NLP.Skladnica.Walenty.Search as Q


------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------


-- | Recursively retrieve all Xml files from the given directory.
getXmlFiles :: FilePath -> IO [FilePath]
getXmlFiles dir = do
  F.find F.always (F.fileName F.~~? "*.xml") dir


-- | Read all verb entries from Walenty and search for them
-- in Skladnica treebank.
runTest
  :: FilePath -- ^ Skladnica directory
  -> FilePath -- ^ Walenty file
  -> FilePath -- ^ Walenty Expansion file
  -> IO ()
runTest skladnicaDir walentyPath expansionPath = do
  expMap <- W.readExpMap expansionPath
  -- read *lexicalized* verbal entries from Walenty
  walenty <-
       S.toList . S.fromList
     . map (W.expandVerb expMap)
     . mapMaybe P.pruneVerb
     . lefts
    <$> W.readWalenty walentyPath
  putStr "Number of lexical entries: " >> print (length walenty)
  -- find all XML files
  xmlFiles <- getXmlFiles skladnicaDir
  -- per each XML file...
  forM_ xmlFiles $ \skladnicaXML -> do
    putStrLn skladnicaXML
    sklForest <- forestFromXml skladnicaXML
    forM_ sklForest $ \sklTree -> do
      -- putStrLn $ showTree sklTree
      forM_ walenty $ \verb -> do
        -- print verb
        -- putStrLn ""
        let expr = Q.querify verb
            mweTrees = Q.markNodes expr sklTree
        E.when (not $ null mweTrees) $ do
          putStrLn "" >> print verb >> putStrLn ""
          -- putStrLn . R.drawForest . map (fmap simpLab . S.simplify) $ mweTrees
          forM_ mweTrees $ \mweTree -> do
            putStrLn . S.drawTree . S.mapFst simpLab . fmap show $ mweTree
  where
    simpLab = L.unpack . either S.cat S.orth . S.label
    -- showTree = R.drawTree . fmap simpLab . S.simplify
    forestFromXml xml = do
      nodesDAG <- S.mkDAG <$> S.readTop xml
      return $ S.forest S.chosen 0 nodesDAG
