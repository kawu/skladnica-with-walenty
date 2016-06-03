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
import           Control.Monad.Morph           as Morph

import qualified System.Random as Random

import Data.PSQueue (Binding(..))
import           Data.Either                   (lefts)
import           Data.IORef
import qualified Data.Map.Strict               as M
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
  , parsedFiles :: S.Set FilePath
  -- ^ Names of the parsed files (i.e., for which at least one
  -- parsed tree has been extracted)
  , seenFiles :: S.Set FilePath
  -- ^ Names of the seen files
  , mweFiles :: S.Set FilePath
  -- ^ Names of the files in which WMEs have been found
  } deriving (Show, Eq, Ord)


showExtract :: Extract -> IO ()
showExtract Extract{..} = do
  putStr "PARSED FILES: " >> print (S.size parsedFiles)
  putStr "SEEN FILES: " >> print (S.size seenFiles)
  putStr "MWE FILES: " >> print (S.size mweFiles)
  putStr "GRAM TREES: " >> print (S.size gramSet)


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
  flip E.execStateT emptyExtract $ do
    forM_ xmlFiles (procPath walenty)
  where
    -- emptyExtract = Extract S.empty 0 0
    emptyExtract = Extract S.empty S.empty S.empty S.empty
    procPath walenty skladnicaXML = do
      E.lift $ putStrLn $ ">>> " ++ skladnicaXML ++ " <<<"
      -- E.modify' $ \st -> st {seenNum = seenNum st + 1}
      E.modify' $ \st -> st {seenFiles = S.insert skladnicaXML (seenFiles st)}
      sklForest <- E.lift $ forestFromXml skladnicaXML
      -- putStrLn "" >> putStrLn "# EXTRACTED:" >> putStrLn ""
      -- printExtracted dag
      let exprs = map Q.querify walenty
      forM_ sklForest $ \sklTree -> do
        let mweTree = Q.markAll exprs sklTree
            sklETs  = G.extractGrammar sklTree
            mweETs = G.extractGrammar mweTree
            est = sklETs `S.union` mweETs
        when (mweTree /= sklTree) $ do
          E.lift $ putStrLn "MWEs found..."
          E.modify' $ \st -> st
            {mweFiles = S.insert skladnicaXML (mweFiles st)}
        when (S.null est) $ do
          E.lift $ putStrLn "WARNING: something went wrong..." -- >> putStrLn ""
        E.modify' $ \st -> st
          -- { parsedNum = parsedNum st + if S.null est then 0 else 1
          { parsedFiles = parsedFiles st `S.union`
                          if S.null est
                          then S.empty
                          else S.singleton skladnicaXML
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


-- | Global stats.
data Stats = Stats
  { mweStatsCheck :: CatStats
  -- ^ Stats on files with Walenty-based MWEs (checkpoint)
  , otherStatsCheck :: CatStats
  -- ^ Stats on files with no Walenty-base MWEs (checkpoint)
  , mweStatsFinal :: CatStats
  -- ^ Stats on files with Walenty-based MWEs (final)
  , otherStatsFinal :: CatStats
  -- ^ Stats on files with no Walenty-base MWEs (final)
  } deriving (Show, Eq, Ord)


-- | Stats per sentence length.
type CatStats = M.Map Int AtomStats


-- | Stats for given sentence length and given files' category.
data AtomStats = AtomStats
  { doneNodes :: Int
  -- ^ Processed hypergraph nodes
  , doneEdges :: Int
  -- ^ Processed hypergraph edges
  , waitNodes :: Int
  -- ^ Waiting hypergraph nodes
  , waitEdges :: Int
  -- ^ Waiting hypergraph edges
  , statsNum  :: Int
  -- ^ Number of stats collected
  } deriving (Show, Eq, Ord)


printAtomStats :: AtomStats -> IO ()
printAtomStats AtomStats{..} = do
  putStr (show $ doneNodes `divide` statsNum) >> putStr ","
  putStr (show $ doneEdges `divide` statsNum) >> putStr ","
  putStr (show $ waitNodes `divide` statsNum) >> putStr ","
  putStr (show $ waitEdges `divide` statsNum) >> putStr ","
  putStrLn (show statsNum)


-- | Empty `AtomStats`
emptyAtomStats :: AtomStats
emptyAtomStats = AtomStats 0 0 0 0 0


atomPlus :: AtomStats -> AtomStats -> AtomStats
atomPlus x y = AtomStats
  { doneNodes = doneNodes x + doneNodes y
  , doneEdges = doneEdges x + doneEdges y
  , waitNodes = waitNodes x + waitNodes y
  , waitEdges = waitEdges x + waitEdges y
  , statsNum = statsNum x + statsNum y }


printCatStats :: CatStats -> IO ()
printCatStats catStats = do
  putStrLn "length,nodes,edges,waitnodes,waitedges,number"
  forM_ (M.toList catStats) $ \(len, atom) -> do
    putStr (show len)
    putStr ","
    printAtomStats atom


printStats :: Stats -> IO ()
printStats Stats{..} = do
  putStrLn "===== MWE FILES (CHECKPOINT) ====="
  printCatStats mweStatsCheck
  putStrLn "===== MWE FILES (FINAL) ====="
  printCatStats mweStatsFinal
  putStrLn "===== OTHER FILES (CHECKPOINT) ====="
  printCatStats otherStatsCheck
  putStrLn "===== OTHER FILES (FINAL) ====="
  printCatStats otherStatsFinal


-- | Empty `Stats`
emptyStats :: Stats
emptyStats = Stats M.empty M.empty M.empty M.empty


updateCatStats :: Int -> AStar.Hype Text Text -> CatStats -> CatStats
updateCatStats sentLen hype =
  M.insertWith atomPlus sentLen $ AtomStats
      { doneNodes = AStar.doneNodesNum hype
      , doneEdges = AStar.doneEdgesNum hype
      , waitNodes = AStar.waitingNodesNum hype
      , waitEdges = AStar.waitingEdgesNum hype
      , statsNum = 1 }


updateMweStatsCheck :: E.MonadState Stats m => Int -> AStar.Hype Text Text -> m ()
updateMweStatsCheck sentLen hype = E.modify' $ \st -> st
  { mweStatsCheck = updateCatStats sentLen hype (mweStatsCheck st) }


updateOtherStatsCheck :: E.MonadState Stats m => Int -> AStar.Hype Text Text -> m ()
updateOtherStatsCheck sentLen hype = E.modify' $ \st -> st
  { otherStatsCheck = updateCatStats sentLen hype (otherStatsCheck st) }


updateMweStatsFinal :: E.MonadState Stats m => Int -> AStar.Hype Text Text -> m ()
updateMweStatsFinal sentLen hype = E.modify' $ \st -> st
  { mweStatsFinal = updateCatStats sentLen hype (mweStatsFinal st) }


updateOtherStatsFinal :: E.MonadState Stats m => Int -> AStar.Hype Text Text -> m ()
updateOtherStatsFinal sentLen hype = E.modify' $ \st -> st
  { otherStatsFinal = updateCatStats sentLen hype (otherStatsFinal st) }


printHypeStats hype = do
  putStr "done nodes: " >> print (AStar.doneNodesNum hype)
  putStr "done edges: " >> print (AStar.doneEdgesNum hype)
  putStr "waiting nodes: " >> print (AStar.waitingNodesNum hype)
  putStr "waiting edges: " >> print (AStar.waitingEdgesNum hype)


-- parsingTest
--   :: FilePath   -- ^ Skladnica directory
--   -> Extract    -- ^ Extracted grammar
--   -> Text       -- ^ Start symbol
--   -> Double     -- ^ Chance to pick a non-MWE file to test
--   -> Maybe Int  -- ^ How many trees to show?
--   -> IO ()
-- parsingTest skladnicaDir Extract{..} begSym pickFile showTrees = do
--   let gram = buildGram gramSet
-- --   let termWei = DAG.termWei gram
-- --   putStrLn ">>> TERM WEI <<<"
-- --   forM_ (M.toList termWei) print
-- --   putStrLn "<<< TERM WEI >>>"
--   xmlFiles <- getXmlFiles skladnicaDir
--   stats <- flip E.execStateT emptyStats $ do
--     forM_ xmlFiles $ \xmlFile -> do
--       -- using `seq` to bypass the space-leak
--       currStats <- E.get
--       length (show currStats) `seq`
--         considerAStar gram xmlFile
--   printStats stats
--   where
--     considerAStar gram skladnicaXML = do
--       i <- liftIO $ Random.randomRIO (0.0, 1.0)
--       when (skladnicaXML `S.member` mweFiles || i <= pickFile) $ do
--         parseAStar gram skladnicaXML
--     parseAStar gram skladnicaXML = do
--       let termMemo = Memo.wrap read show $ Memo.list Memo.char
--           auto = AStar.mkAuto termMemo gram
--           dag = DAG.dagGram gram
--       liftIO $ putStrLn $ ">>> " ++ skladnicaXML ++ " <<<"
--       sklForest <- liftIO $ forestFromXml skladnicaXML
--       forM_ sklForest $ \sklTree -> do
--         let sent = baseForms sklTree
--             input = AStar.fromList sent
--             -- below we make the AStar pipe work in the State monad
--             -- transformer over IO (normally it works just over IO)
--             pipe = Morph.hoist E.lift $ AStar.earleyAutoP auto input
--             sentLen = length sent
--             final p = AStar._spanP p == AStar.Span 0 sentLen Nothing
--                    && AStar._dagID p == Left begSym
--         contRef <- E.lift $ newIORef True
--         hype <- runEffect . for pipe $ \(item, hype) -> void . runMaybeT $ do
--           E.guard =<< liftIO (readIORef contRef)
--           AStar.ItemP p <- return item
--           E.guard (final p)
--           liftIO $ putStrLn "<<CHECKPOINT>>" >> printHypeStats hype >> putStrLn ""
--           if skladnicaXML `S.member` mweFiles
--             then updateMweStatsCheck sentLen hype
--             else updateOtherStatsCheck sentLen hype
--           liftIO $ case showTrees of
--               Nothing -> return ()
--               Just k -> do
--                 mapM_
--                   (putStrLn . R.drawTree . fmap show . T.encode . Left)
--                   (take k . nubOrd $ AStar.fromPassive p hype)
--                 putStrLn "<<DERIVATIONS>>"
--                 ds <- AStar.derivFromPassive p hype input
--                 let ds' = map
--                       (AStar.expandDeriv dag . AStar.deriv2tree)
--                       (lefts $ S.toList ds)
--                 mapM_
--                   (putStrLn . R.drawTree . fmap show)
--                   (take k ds')
--           liftIO $ writeIORef contRef False
--         liftIO $ putStrLn "<<FINISH>>" >> printHypeStats hype
--         -- putStr "deriv num: " >> print (length ts)
--         -- putStr "tree num: "  >> print (length $ nubOrd ts)  >> putStrLn ""
--         if skladnicaXML `S.member` mweFiles
--           then updateMweStatsFinal sentLen hype
--           else updateOtherStatsFinal sentLen hype
--         liftIO $ case showTrees of
--           Nothing -> return ()
--           Just k -> do
--             let ts = AStar.parsedTrees hype begSym sentLen
--             mapM_
--               (putStrLn . R.drawTree . fmap show . T.encode . Left)
--               (take k $ nubOrd ts)
--             putStrLn "<<DERIVATIONS>>"
--             ds <- AStar.derivTrees hype begSym input
--             let ds' = map (AStar.expandDeriv dag . AStar.deriv2tree) (S.toList ds)
--             mapM_
--               (putStrLn . R.drawTree . fmap show)
--               (take k ds')
-- --     procPath dag skladnicaXML = do
-- --       putStrLn $ ">>> " ++ skladnicaXML ++ " <<<"
-- --       sklForest <- forestFromXml skladnicaXML
-- --       forM_ sklForest $ \sklTree -> do
-- --         putStr "Can recognize: "
-- --         print =<< recognize dag (baseForms sklTree)


data Result a
  = None
  | Some a
  | Done
  deriving (Show, Eq, Ord)


parsingTest
  :: FilePath   -- ^ Skladnica directory
  -> Extract    -- ^ Extracted grammar
  -> Text       -- ^ Start symbol
  -> Double     -- ^ Chance to pick a non-MWE file to test
  -> Maybe Int  -- ^ How many trees to show?
  -> IO ()
parsingTest skladnicaDir Extract{..} begSym pickFile showTrees = do
  let gram = buildGram gramSet
  xmlFiles <- getXmlFiles skladnicaDir
  stats <- flip E.execStateT emptyStats $ do
    forM_ xmlFiles $ \xmlFile -> do
      -- using `seq` to bypass the space-leak
      currStats <- E.get
      length (show currStats) `seq`
        considerAStar gram xmlFile
  printStats stats
  where
    considerAStar gram skladnicaXML = do
      i <- liftIO $ Random.randomRIO (0.0, 1.0)
      when (skladnicaXML `S.member` mweFiles || i <= pickFile) $ do
        parseAStar gram skladnicaXML
    parseAStar gram skladnicaXML = do
      let termMemo = Memo.wrap read show $ Memo.list Memo.char
          auto = AStar.mkAuto termMemo gram
      liftIO $ putStrLn $ ">>> " ++ skladnicaXML ++ " <<<"
      sklForest <- liftIO $ forestFromXml skladnicaXML
      forM_ sklForest $ \sklTree -> do
        let sent = baseForms sklTree
            input = AStar.fromList sent
            -- below we make the AStar pipe work in the State monad
            -- transformer over IO (normally it works just over IO)
            pipe = Morph.hoist E.lift $ AStar.earleyAutoP auto input
            sentLen = length sent
            final p = AStar._spanP p == AStar.Span 0 sentLen Nothing
                   && AStar._dagID p == Left begSym
        contRef <- E.lift $ newIORef None
        hype <- runEffect . for pipe $ \(item :-> itemWeight, hype) -> void . runMaybeT $ do
          cont <- liftIO (readIORef contRef)
          case cont of
            None -> do
              AStar.ItemP p <- return item
              E.guard (final p)
              liftIO $ putStrLn "<<BEGIN>>" >> printHypeStats hype >> putStrLn ""
              liftIO $ writeIORef contRef (Some $ getWeight itemWeight)
            Some optimal -> do
              -- waiting for the first time that the optimal weight is surpassed
              E.guard $ getWeight itemWeight > optimal
              liftIO $ putStrLn "<<CHECKPOINT>>" >> printHypeStats hype >> putStrLn ""
              if skladnicaXML `S.member` mweFiles
                then updateMweStatsCheck sentLen hype
                else updateOtherStatsCheck sentLen hype
              liftIO $ writeIORef contRef Done
            Done -> return ()
        liftIO $ putStrLn "<<FINISH>>" >> printHypeStats hype
        if skladnicaXML `S.member` mweFiles
          then updateMweStatsFinal sentLen hype
          else updateOtherStatsFinal sentLen hype
    getWeight e = AStar.priWeight e + AStar.estWeight e


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
  parsingTest skladnicaDir extr "wypowiedzenie" 0.0 Nothing -- (Just 1)


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


divide :: (Integral a, Integral b) => a -> b -> Double
divide x y = (fromIntegral x :: Double) / fromIntegral y


nubOrd :: (Ord a) => [a] -> [a]
nubOrd = S.toList . S.fromList
