{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


-- | A module responsible for identifying occurrences of the
-- Walenty valency dictionary entries in the Skladnica treebank.


module NLP.Skladnica.Walenty
( runTest
, extractGrammar
, parsingTest
, fullTest
) where


import           Control.Monad                 (forM_, when)
import qualified Control.Monad.State.Strict    as E
import           Control.Monad.Trans.Maybe     (MaybeT (..))

import           Data.Either                   (lefts)
import           Data.IORef
import           Data.Maybe                    (mapMaybe)
import qualified Data.MemoCombinators          as Memo
import qualified Data.Set                      as S
import           Data.Text.Lazy                (Text)
import qualified Data.Text.Lazy                as L
import           Data.Tree                     as R

import           Pipes
import qualified Pipes.Prelude                 as Pipes

import qualified System.FilePath.Find          as F

import qualified NLP.Partage.AStar             as AStar
import qualified NLP.Partage.DAG               as DAG
import qualified NLP.Partage.Earley            as Earley
import qualified NLP.Partage.Tree.Other        as T

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
      -- procVerbs sklTree walenty
      forM_ walenty (procVerb sklTree)
  where
    simpLab = L.unpack . either S.cat S.orth . S.label
    procVerb sklTree verb = do
      -- print verb
      -- putStrLn ""
      let expr = Q.querify verb
          mweTrees = Q.markNodes expr sklTree
      when (not $ null mweTrees) $ do
        putStrLn "" >> print verb >> putStrLn ""
        -- putStrLn . R.drawForest . map (fmap simpLab . S.simplify) $ mweTrees
        forM_ mweTrees $ \mweTree -> do
          putStrLn . S.drawTree . S.mapFst simpLab . fmap show $ mweTree
    procVerbs sklTree verbs = do
      let exprs = map Q.querify verbs
          sklTree' = Q.markAll exprs sklTree
      when (sklTree /= sklTree') $ do
        putStrLn . S.drawTree . S.mapFst simpLab . fmap show $ sklTree
        putStrLn . S.drawTree . S.mapFst simpLab . fmap show $ sklTree'


------------------------------------------------------------------------------
-- Grammar Extraction Tests
--------------------------------------------------------------------------------


data Extract = Extract
  { gramSet   :: S.Set G.ET
  -- ^ The resulting grammar
  , seenNum   :: Int
  -- ^ The number of seen files
  , parsedNum :: Int
  -- ^ The number of parsed files (i.e., for which at least one
  -- parsed tree has been extracted)
  } deriving (Show, Eq, Ord)


showExtract :: Extract -> IO ()
showExtract Extract{..} = do
  putStr "SEEN: " >> print seenNum
  putStr "PARSED: " >> print parsedNum
  putStr "GRAM TREES: " >> print (S.size gramSet)


-- -- | Read all verb entries from Walenty and search for them
-- -- in Skladnica treebank.
-- -- Extract the grammar from the resulting syntactic trees
-- -- (both with and without MWEs).
-- runExtraction
--   :: FilePath -- ^ Skladnica directory
--   -> FilePath -- ^ Walenty file
--   -> FilePath -- ^ Walenty expansion file
--   -> IO ()
-- runExtraction skladnicaDir walentyPath expansionPath = do
--   -- read *lexicalized* verbal entries from Walenty
--   walenty <- readWalenty walentyPath expansionPath
--   putStr "Number of lexical entries: " >> print (length walenty)
--   -- find all XML files
--   xmlFiles <- getXmlFiles skladnicaDir
--   -- per each XML file...
--   walkStats <- flip E.execStateT emptyStats $ do
--     forM_ xmlFiles (procPath walenty)
--   showExtract walkStats
--   putStrLn ""
--   forM_ (S.toList $ gramSet walkStats) $
--     putStrLn . R.drawTree . fmap show
--   where
--     emptyStats = Extract S.empty 0 0
--     procPath walenty skladnicaXML = do
--       E.lift $ putStrLn $ ">>> " ++ skladnicaXML ++ " <<<"
--       E.modify' $ \st -> st {seenNum = seenNum st + 1}
--       sklForest <- E.lift $ forestFromXml skladnicaXML
--       -- putStrLn "" >> putStrLn "# EXTRACTED:" >> putStrLn ""
--       -- printExtracted dag
--       let exprs = map Q.querify walenty
--       forM_ sklForest $ \sklTree -> do
--         let mweTree = Q.markAll exprs sklTree
--             sklETs  = G.topShatter . G.prepTree . S.mapFst S.label $ sklTree
--             mweETs = G.topShatter . G.prepTree . S.mapFst S.label $ mweTree
--             est = sklETs `S.union` mweETs
--         when (mweTree /= sklTree) $ do
--           E.lift $ putStrLn "MWEs found..."
--         when (S.null est) $ do
--           E.lift $ putStrLn "WARNING: something went wrong..." -- >> putStrLn ""
--         E.modify' $ \st -> st
--           { parsedNum = parsedNum st + if S.null est then 0 else 1
--           , gramSet = gramSet st `S.union` est }
--         when (not $ S.null est) $ do
--           g <- E.gets gramSet
--           let trees = map (fmap show) (S.toList est)
--           length (R.drawForest trees) `seq` E.lift $ do
--             putStr "Current number of trees: "
--             print $ S.size g
--         let realMweETs = mweETs `S.difference` sklETs
--         when (not $ S.null realMweETs) $ do
--           E.lift $ putStrLn "MWE elementary trees found:\n"
--           let trees = map (fmap show) (S.toList realMweETs)
--           E.lift $ putStrLn $ R.drawForest trees
--         -- recognition tests
--         currGram <- E.gets gramSet
--         let dag = buildGram currGram
--         E.lift $ do
--           putStr "Can recognize: "
--           print =<< recognize dag (baseForms sklTree)


-- | Read all verb entries from Walenty and search for them
-- in Skladnica treebank.
-- Extract the grammar from the resulting syntactic trees
-- (both with and without MWEs).
extractGrammar
  :: FilePath -- ^ Skladnica directory
  -> FilePath -- ^ Walenty file
  -> FilePath -- ^ Walenty expansion file
  -> IO Extract
extractGrammar skladnicaDir walentyPath expansionPath = do
  -- read *lexicalized* verbal entries from Walenty
  walenty <- readWalenty walentyPath expansionPath
  putStr "Number of lexical entries: " >> print (length walenty)
  -- find all XML files
  xmlFiles <- getXmlFiles skladnicaDir
  -- per each XML file...
  flip E.execStateT emptyStats $ do
    forM_ xmlFiles (procPath walenty)
  where
    emptyStats = Extract S.empty 0 0
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
        when (mweTree /= sklTree) $ do
          E.lift $ putStrLn "MWEs found..."
        when (S.null est) $ do
          E.lift $ putStrLn "WARNING: something went wrong..." -- >> putStrLn ""
        E.modify' $ \st -> st
          { parsedNum = parsedNum st + if S.null est then 0 else 1
          , gramSet = gramSet st `S.union` est }
        when (not $ S.null est) $ do
          g <- E.gets gramSet
          let trees = map (fmap show) (S.toList est)
          length (R.drawForest trees) `seq` E.lift $ do
            putStr "Current number of trees: "
            print $ S.size g
        let realMweETs = mweETs `S.difference` sklETs
        when (not $ S.null realMweETs) $ do
          E.lift $ putStrLn "MWE elementary trees found:\n"
          let trees = map (fmap show) (S.toList realMweETs)
          E.lift $ putStrLn $ R.drawForest trees


-- | Build a grammar from the given set of ETs.
buildGram
  :: S.Set G.ET
  -> DAG.Gram Text Text
buildGram = DAG.mkGram . map (,1) . S.toList


-- | Check if the given sentence is recognized with the
-- given TAG grammar.
recognize
  :: DAG.Gram Text Text -- ^ The grammar
  -> [Text] -- ^ The sentence
  -> IO Bool
recognize dag = Earley.recognize dag . Earley.fromList


-- | Retrieve terminal base forms from the given syntactic tree.
baseForms
  :: Q.SklTree
  -> [Text]
baseForms = map S.base . Q.terminals


------------------------------------------------------------------------------
-- TAG Parsing Tests
--------------------------------------------------------------------------------


-- data ParseStats = ParseStats
--   { gramSet   :: S.Set G.ET
--   -- ^ The resulting grammar
--   , seenNum   :: Int
--   -- ^ The number of seen files
--   , parsedNum :: Int
--   -- ^ The number of parsed files (i.e., for which at least one
--   -- parsed tree has been extracted)
--   } deriving (Show, Eq, Ord)


parsingTest
  :: FilePath   -- ^ Skladnica directory
  -> S.Set G.ET -- ^ Extracted grammar
  -> Text       -- ^ Start symbol
  -> Bool       -- ^ Show trees?
  -> IO ()
parsingTest skladnicaDir gramSet begSym showTrees = do
  let gram = buildGram gramSet
      dag = DAG.dagGram gram
      auto = AStar.mkAuto termMemo gram
  xmlFiles <- getXmlFiles skladnicaDir
  forM_ xmlFiles (parseAStar dag auto)
  where
    termMemo = Memo.wrap read show $ Memo.list Memo.char
    -- parseAStar :: DAG.Gram Text Text -> AStar.Auto Text Text -> FilePath -> IO ()
    parseAStar dag auto skladnicaXML = do
      putStrLn $ ">>> " ++ skladnicaXML ++ " <<<"
      sklForest <- forestFromXml skladnicaXML
      forM_ sklForest $ \sklTree -> do
        let sent = baseForms sklTree
            input = AStar.fromList sent
            pipe = AStar.earleyAutoP auto input
            sentLen = length sent
            final p = AStar._spanP p == AStar.Span 0 sentLen Nothing
                   && AStar._dagID p == Left begSym
        contRef <- newIORef True
        hype <- runEffect . for pipe $ \(item, hype) -> void . runMaybeT $ do
          E.guard =<< liftIO (readIORef contRef)
          AStar.ItemP p <- return item
          E.guard (final p)
          liftIO $ do
            putStrLn "<<CHECKPOINT>>" >> printStats hype >> putStrLn ""
            when showTrees $ mapM_
              (putStrLn . R.drawTree . fmap show . T.encode . Left)
              (nubOrd $ AStar.fromPassive p hype)
          liftIO $ writeIORef contRef False
        putStrLn "<<FINISH>>" >> printStats hype
        let ts = AStar.parsedTrees hype begSym sentLen
        -- putStr "deriv num: " >> print (length ts)
        -- putStr "tree num: "  >> print (length $ nubOrd ts)  >> putStrLn ""
        when showTrees $ mapM_
          (putStrLn . R.drawTree . fmap show . T.encode . Left)
          (nubOrd ts)
        when showTrees $ do
          putStrLn "<<DERIVATIONS>>"
          ds <- AStar.derivTrees hype begSym input
          let ds' = map (AStar.expandDeriv dag . AStar.deriv2tree) (S.toList ds)
          mapM_ (putStrLn . R.drawTree . fmap show) ds'
    printStats hype = do
      putStr "done nodes: " >> print (AStar.doneNodesNum hype)
      putStr "done edges: " >> print (AStar.doneEdgesNum hype)
      putStr "waiting nodes: " >> print (AStar.waitingNodesNum hype)
      putStr "waiting edges: " >> print (AStar.waitingEdgesNum hype)
--     procPath dag skladnicaXML = do
--       putStrLn $ ">>> " ++ skladnicaXML ++ " <<<"
--       sklForest <- forestFromXml skladnicaXML
--       forM_ sklForest $ \sklTree -> do
--         putStr "Can recognize: "
--         print =<< recognize dag (baseForms sklTree)


fullTest
  :: FilePath -- ^ Skladnica directory
  -> FilePath -- ^ Walenty file
  -> FilePath -- ^ Walenty expansion file
  -> IO ()
fullTest skladnicaDir walentyPath expansionPath = do
  putStrLn "\n===== GRAMMAR EXTRACTION =====\n"
  extr <- extractGrammar skladnicaDir walentyPath expansionPath
  putStrLn "\n===== EXTRACTED GRAMMAR =====\n"
  showExtract extr
  putStrLn ""
  forM_ (S.toList $ gramSet extr) $
    putStrLn . R.drawTree . fmap show
  putStrLn "\n===== PARSING TESTS =====\n"
  parsingTest skladnicaDir (gramSet extr) "wypowiedzenie" False


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
forestFromXml :: FilePath -> IO [Q.SklTree]
forestFromXml xml = do
  nodesDAG <- S.mkDAG <$> S.readTop xml
  return $ S.forest S.chosen 0 nodesDAG


--------------------------------------------------
-- Misc
--------------------------------------------------


-- divide :: (Integral a, Integral b) => a -> b -> Double
-- divide x y = (fromIntegral x :: Double) / fromIntegral y


nubOrd :: (Ord a) => [a] -> [a]
nubOrd = S.toList . S.fromList
