{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


-- | A module responsible for identifying occurrences of the
-- Walenty valency dictionary entries in the Skladnica treebank.


module NLP.Skladnica.Walenty
( runTest
, runExtraction
) where


import           Control.Monad                 (forM_)
import qualified Control.Monad.State.Strict    as E

import           Data.Either                   (lefts)
import           Data.Maybe                    (mapMaybe)
import qualified Data.Set                      as S
import qualified Data.Text.Lazy                as L
import           Data.Tree                     as R

import qualified System.FilePath.Find          as F

import qualified NLP.Skladnica                 as S
import qualified NLP.Walenty                   as W

import qualified NLP.Skladnica.Walenty.Grammar as G
import qualified NLP.Skladnica.Walenty.Prune   as P
import qualified NLP.Skladnica.Walenty.Search  as Q


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
  -- read *lexicalized* verbal entries from Walenty
  walenty <- readWalenty walentyPath expansionPath
  putStr "Number of lexical entries: " >> print (length walenty)
  -- find all XML files
  xmlFiles <- getXmlFiles skladnicaDir
  -- per each XML file...
  forM_ xmlFiles $ \skladnicaXML -> do
    putStrLn skladnicaXML
    sklForest <- forestFromXml skladnicaXML
    forM_ sklForest $ \sklTree -> do
      -- putStrLn $ showTree sklTree
      procVerbs sklTree walenty
      -- forM_ walenty (procVerb sklTree)
  where
    simpLab = L.unpack . either S.cat S.orth . S.label
--     procVerb sklTree verb = do
--       -- print verb
--       -- putStrLn ""
--       let expr = Q.querify verb
--           mweTrees = Q.markNodes expr sklTree
--       E.when (not $ null mweTrees) $ do
--         putStrLn "" >> print verb >> putStrLn ""
--         -- putStrLn . R.drawForest . map (fmap simpLab . S.simplify) $ mweTrees
--         forM_ mweTrees $ \mweTree -> do
--           putStrLn . S.drawTree . S.mapFst simpLab . fmap show $ mweTree
    procVerbs sklTree verbs = do
      let exprs = map Q.querify verbs
          sklTree' = Q.markAll exprs sklTree
      E.when (sklTree /= sklTree') $ do
        putStrLn . S.drawTree . S.mapFst simpLab . fmap show $ sklTree
        putStrLn . S.drawTree . S.mapFst simpLab . fmap show $ sklTree'


------------------------------------------------------------------------------
-- Grammar Extraction Tests
--------------------------------------------------------------------------------


data ExtractStats = ExtractStats
  { gramSet   :: S.Set G.ET
  -- ^ The resulting grammar
  , seenNum   :: Int
  -- ^ The number of seen files
  , parsedNum :: Int
  -- ^ The number of parsed files (i.e., for which at least one
  -- parsed tree has been extracted)
  } deriving (Show, Eq, Ord)


showWalkStats ExtractStats{..} = do
  putStr "SEEN: " >> print seenNum
  putStr "PARSED: " >> print parsedNum
  putStr "GRAM TREES: " >> print (S.size gramSet)


-- | Read all verb entries from Walenty and search for them
-- in Skladnica treebank.
runExtraction
  :: FilePath -- ^ Skladnica directory
  -> FilePath -- ^ Walenty file
  -> FilePath -- ^ Walenty expansion file
  -> IO ()
runExtraction skladnicaDir walentyPath expansionPath = do
  -- read *lexicalized* verbal entries from Walenty
  walenty <- readWalenty walentyPath expansionPath
  putStr "Number of lexical entries: " >> print (length walenty)
  -- find all XML files
  xmlFiles <- getXmlFiles skladnicaDir
  -- per each XML file...
  walkStats <- flip E.execStateT emptyStats $ do
    forM_ xmlFiles (procPath walenty)
  showWalkStats walkStats
  forM_ (S.toList $ gramSet walkStats) $
    putStrLn . R.drawTree . fmap show
  where
    emptyStats = ExtractStats S.empty 0 0
    procPath walenty skladnicaXML = do
      E.lift $ putStrLn $ ">>> " ++ skladnicaXML ++ " <<<"
      E.modify' $ \st -> st {seenNum = seenNum st + 1}
      sklForest <- E.lift $ forestFromXml skladnicaXML
      -- putStrLn "" >> putStrLn "# EXTRACTED:" >> putStrLn ""
      -- printExtracted dag
      let exprs = map Q.querify walenty
      forM_ sklForest $ \sklTree -> do
        let mweTree = Q.markAll exprs sklTree
            sklETs  = G.topShatter . G.prepTree . S.mapFst S.label $ sklTree
            mweETs = G.topShatter . G.prepTree . S.mapFst S.label $ mweTree
            est = sklETs `S.union` mweETs
        E.when (mweTree /= sklTree) $ do
          E.lift $ putStrLn "MWEs found..."
        E.when (S.null est) $ do
          E.lift $ putStrLn "Something went wrong..." -- >> putStrLn ""
        E.modify' $ \st -> st
          { parsedNum = parsedNum st + if S.null est then 0 else 1
          , gramSet = gramSet st `S.union` est }
        E.when (not $ S.null est) $ do
          g <- E.gets gramSet
          let trees = map (fmap show) (S.toList est)
          length (R.drawForest trees) `seq` E.lift $ do
            putStr "Current number of trees: "
            print $ S.size g
        let realMweETs = mweETs `S.difference` sklETs
        E.when (not $ S.null realMweETs) $ do
          E.lift $ putStrLn "MWE elementary trees found:\n"
          let trees = map (fmap show) (S.toList realMweETs)
          E.lift $ putStrLn $ R.drawForest trees


------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------


-- readWalenty
--   :: FilePath -- ^ Walenty filePath
--   -> FilePath -- ^ Expansion file
--   -> IO ()
readWalenty walentyPath expansionPath = do
  expMap <- W.readExpMap expansionPath
  -- read *lexicalized* verbal entries from Walenty
  walenty <-
       S.toList . S.fromList
     . map (W.expandVerb expMap)
     . mapMaybe P.pruneVerb
     . lefts
    <$> W.readWalenty walentyPath
  return walenty


-- | Extract skladnica trees from the given XML file.
forestFromXml :: FilePath -> IO [Q.SklTree]
forestFromXml xml = do
  nodesDAG <- S.mkDAG <$> S.readTop xml
  return $ S.forest S.chosen 0 nodesDAG
