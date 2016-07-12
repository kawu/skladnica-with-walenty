{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module NLP.Skladnica.New
( GlobalCfg (..)
, SelectCfg (..)
, compileSelect
, runExperiment
) where


import           Control.Monad                 (forM_, guard, void)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Morph           as Morph
import qualified Control.Monad.State.Strict    as E
import           Control.Monad.Trans.Maybe     (MaybeT (..))

import           Data.IORef
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Tree                     as R

import           Pipes

import qualified NLP.Partage.AStar             as AStar
import qualified NLP.Partage.AStar.Deriv       as Deriv

import qualified NLP.Skladnica.Extract         as Ext
import qualified NLP.Skladnica.Walenty.Grammar as G
import qualified NLP.Skladnica.Walenty.MweTree as MWE
import qualified NLP.Skladnica.Walenty.Select  as Select


-- | Experiment configuration
data GlobalCfg = GlobalCfg
  { skladnicaXML    :: FilePath
    -- ^ MWE-marked Składnica treebank
  , restrictGrammar :: Bool
    -- ^ Restrict (supertag) the grammar to match
    -- the individual sentences?
  , useFreqs        :: Bool
    -- ^ Use frequencies to customize heuristic?
  , begSym          :: Text
    -- ^ Start symbol
  , maxDerivNum     :: Int
    -- ^ Maximum number of derivation to generate to find the
    -- derivation corresponding to the given Składnica tree
  , termTyp         :: Ext.TermType
    -- ^ What type of terminals use in the experiments
  , selectCfg       :: SelectCfg
    -- ^ MWE selection configuration
  }
  deriving (Show)


-- | A custom datatype which facilitates computing the size
-- of the "optimal" part of the hypergraph.
data Result a
  = None
  | Some a
  | Done
  deriving (Show, Eq, Ord)


-- | Run our full experiment.
runExperiment :: GlobalCfg -> IO ()
runExperiment GlobalCfg{..} = do

  -- MWE selection predicate
  let mweSelect = compileSelect selectCfg

  putStrLn "\n===== GRAMMAR EXTRACTION =====\n"
  extract <- Ext.fromFile termTyp mweSelect skladnicaXML

  putStrLn "\n===== EXTRACTED GRAMMAR =====\n"
  forM_ (S.toList $ Ext.gramSet extract) $
    putStrLn . R.drawTree . fmap show

  -- grammar-building function
  let buildGram = if useFreqs
                  then Ext.buildFreqGram (Ext.freqMap extract)
                  else Ext.buildGram

  -- single global grammar for all
  let globGram = buildGram (Ext.gramSet extract)

  putStrLn "\n===== PARSING TESTS =====\n"
  skladnica <- MWE.readTop skladnicaXML

  putStr "file-name,sent-length,reg-deriv-size,mwe-deriv-size,"
  putStr "chart-nodes-1,chart-arcs-1,agenda-nodes-1,agenda-arcs-1,"
  putStr "encodes-reg-1,encodes-mwe-1,"
  putStr "chart-nodes-2,chart-arcs-2,agenda-nodes-2,agenda-arcs-2,"
  putStr "encodes-reg-2,encodes-mwe-2"
  putStrLn ""

  -- flip E.execStateT () $ forM_ skladnica $ \sklTree0 -> do
  forM_ skladnica $ \sklTree0 -> runMaybeT $ do

    -- First we construct two versions of the syntactic tree: one compositional,
    -- one which assumes MWE interpretations.
    let sklTree = fmap MWE.sklNode $ MWE.topRoot sklTree0
        mweTree = fmap MWE.sklNode . MWE.emboss mweSelect $ MWE.topRoot sklTree0

    -- Stop if the two versions are identical.
    guard $ mweTree /= sklTree

    -- Some utility "variables"
    let sent = Ext.wordForms termTyp sklTree
        sentLen = length sent
        final p = AStar._spanP p == AStar.Span 0 sentLen Nothing
               && AStar._dagID p == Left begSym
        getWeight e = AStar.priWeight e + AStar.estWeight e

    -- Find the reference derivation trees corresponding to the syntactic trees
    refRegDeriv <- MaybeT $ Ext.findDeriv maxDerivNum begSym termTyp sklTree
    refMweDeriv <- MaybeT $ Ext.findDeriv maxDerivNum begSym termTyp mweTree

    -- Don't need any more MaybeT capabilities
    lift $ do

      -- Column: file name (if in meta-attributes)
      let fileName = T.map escComma . maybe "_" id . M.lookup "file"
          escComma c = case c of ',' -> '.' ; _ -> c
      liftIO $ T.putStr (fileName $ MWE.topAtts sklTree0)

      -- Column: sentence length
      liftIO $ putStr "," >> putStr (show sentLen)

      -- Columns: reg-deriv-size and mwe-deriv-size
      liftIO $ putStr "," >> putStr (show $ Ext.derivSize refRegDeriv)
      liftIO $ putStr "," >> putStr (show $ Ext.derivSize refMweDeriv)

      -- Build the local grammar (simple form of super-tagging)
      let localETs = Select.select (S.fromList sent) (Ext.gramSet extract)
          localGram = if restrictGrammar then buildGram localETs else globGram

      -- Used to control the state of the parsing process
      contRef <- liftIO $ newIORef None

      -- We have to hoist the parsing pipe to `MaybeT`; UPDATE: not anymore
      -- let pipe = Morph.hoist E.lift $ Ext.parsePipe sent begSym localGram
      let pipe = Ext.parsePipe sent begSym localGram
      hypeFini <- runEffect . for pipe $ \(hypeModif, _derivTrees) -> do
        let item = AStar.modifItem hypeModif
            itemWeight = AStar.modifTrav hypeModif
            hype = AStar.modifHype hypeModif
        void . runMaybeT $ do
          cont <- liftIO (readIORef contRef)
          case cont of
            None -> do
              AStar.ItemP p <- return item
              E.guard (final p)
              liftIO . writeIORef contRef . Some $ getWeight itemWeight
            Some optimal -> do
              guard $ getWeight itemWeight > optimal
              -- the first time that the optimal weight is surpassed
              liftIO $ do
                writeIORef contRef Done
                -- Columns: hype stats at checkpoint 1
                printHypeStats hype
                -- Column: are ref. derivations encoded in the graph
                let encodes = Deriv.encodes hype begSym sentLen
                putStr $ "," ++ if encodes refRegDeriv then "1" else "0"
                putStr $ "," ++ if encodes refMweDeriv then "1" else "0"
            Done -> return ()

      -- Columns: hype stats at the end, are ref. derivations encoded in the graph
      liftIO $ do
        printHypeStats hypeFini
        let encodes = Deriv.encodes hypeFini begSym sentLen
        putStr $ "," ++ if encodes refRegDeriv then "1" else "0"
        putStr $ "," ++ if encodes refMweDeriv then "1" else "0"
        putStrLn ""


-- -- | Run our full experiment.
-- runExperiment :: GlobalCfg -> IO ()
-- runExperiment GlobalCfg{..} = do
--
--   putStrLn "\n===== GRAMMAR EXTRACTION =====\n"
--   extract <- Ext.fromFile termTyp skladnicaXML
--
--   putStrLn "\n===== EXTRACTED GRAMMAR =====\n"
--   forM_ (S.toList $ Ext.gramSet extract) $
--     putStrLn . R.drawTree . fmap show
--
--   -- grammar-building function
--   let buildGram = if useFreqs
--                   then Ext.buildFreqGram (Ext.freqMap extract)
--                   else Ext.buildGram
--
--   -- single global grammar for all
--   let globGram = buildGram (Ext.gramSet extract)
--
--   putStrLn "\n===== PARSING TESTS =====\n"
--   skladnica <- MWE.readTop skladnicaXML
--
--   putStr "sent-length,reg-deriv-size,mwe-deriv-size,"
--   putStr "chart-nodes-1,chart-arcs-1,agenda-nodes-1,agenda-arcs-1,"
--   putStr "encodes-reg-1,encodes-mwe-1,"
--   putStr "chart-nodes-2,chart-arcs-2,agenda-nodes-2,agenda-arcs-2,"
--   putStr "encodes-reg-2,encodes-mwe-2"
--   putStrLn ""
--
--   -- flip E.execStateT () $ forM_ skladnica $ \sklTree0 -> do
--   forM_ skladnica $ \sklTree0 -> runMaybeT $ do
--
--     -- First we construct two versions of the syntactic tree: one compositional,
--     -- one which assumes MWE interpretations.
--     let sklTree = fmap MWE.sklNode sklTree0
--         mweTree = fmap MWE.sklNode (MWE.emboss sklTree0)
--
--     -- Stop if the two versions are identical.
--     guard $ mweTree /= sklTree
--
--     -- Some utility "variables"
--     let sent = Ext.wordForms termTyp sklTree
--         sentLen = length sent
--         final p = AStar._spanP p == AStar.Span 0 sentLen Nothing
--                && AStar._dagID p == Left begSym
--         getWeight e = AStar.priWeight e + AStar.estWeight e
--
--     -- DEBUG: sentence
--     liftIO . T.putStrLn $ T.unwords sent
--
--     -- Column: sentence length
--     liftIO $ putStr (show sentLen)
--
--     -- Find the reference derivation trees corresponding to the syntactic trees
--     refRegDeriv <- MaybeT $ Ext.findDeriv maxDerivNum begSym termTyp sklTree
--     refMweDeriv <- MaybeT $ Ext.findDeriv maxDerivNum begSym termTyp mweTree
--
--     -- DEBUG:
--     let putRose = putStrLn . R.drawTree . fmap show
--         refDepTree = Dep.fromDeriv . Gorn.fromDeriv $ refMweDeriv
--     liftIO . putRose . Dep.toRose $ refDepTree
--
-- --     -- Columns: reg-deriv-size and mwe-deriv-size
-- --     liftIO $ putStr "," >> putStr (show $ Ext.derivSize refRegDeriv)
-- --     liftIO $ putStr "," >> putStr (show $ Ext.derivSize refMweDeriv)
--
--     -- Build the local grammar (simple form of super-tagging)
--     let localETs = Select.select (S.fromList sent) (Ext.gramSet extract)
--         localGram = if restrictGrammar then buildGram localETs else globGram
--
--     -- Used to control the state of the parsing process
--     contRef <- liftIO $ newIORef None
--
--     -- We have to hoist the parsing pipe to `MaybeT`
--     let pipe = Morph.hoist E.lift $ Ext.parsePipe sent begSym localGram
--     hypeFini <- runEffect . for pipe $ \(hypeModif, _derivTrees) -> do
--       let item = AStar.modifItem hypeModif
--           itemWeight = AStar.modifTrav hypeModif
--           hype = AStar.modifHype hypeModif
--       void . runMaybeT $ do
--         cont <- liftIO (readIORef contRef)
--         case cont of
--           None -> do
--             AStar.ItemP p <- return item
--             E.guard (final p)
--             liftIO . writeIORef contRef . Some $ getWeight itemWeight
--           Some optimal -> do
--             guard $ getWeight itemWeight > optimal
--             -- the first time that the optimal weight is surpassed
--             liftIO $ do
--               writeIORef contRef Done
-- --               -- Columns: hype stats at checkpoint 1
-- --               printHypeStats hype
--               -- Column: are ref. derivations encoded in the graph
--               let encodes = Deriv.encodes hype begSym sentLen
-- --               putStr $ "," ++ if encodes refRegDeriv then "1" else "0"
-- --               putStr $ "," ++ if encodes refMweDeriv then "1" else "0"
--               if encodes refMweDeriv
--                 then return ()
--                 else do
--                   let curDerTree = head $ Deriv.derivTrees hype begSym sentLen
--                       curDepTree = Dep.fromDeriv . Gorn.fromDeriv $ curDerTree
--                   liftIO . putRose . Dep.toRose $ curDepTree
--           Done -> return ()
--
--     -- Columns: hype stats at the end, are ref. derivations encoded in the graph
--     liftIO $ do
--       printHypeStats hypeFini
--       let encodes = Deriv.encodes hypeFini begSym sentLen
--       putStr $ "," ++ if encodes refRegDeriv then "1" else "0"
--       putStr $ "," ++ if encodes refMweDeriv then "1" else "0"
--       putStrLn ""



------------------------------------------------------------------------------
-- Selecting MWEs
--------------------------------------------------------------------------------


-- | MWE selection configuration
data SelectCfg = SelectCfg
  { noWalenty :: Bool
    -- ^ Discard MWEs from Walenty
  , noSejf    :: Bool
    -- ^ Discard MWEs from Sejf
  , noNKJP    :: Bool
    -- ^ Discard MWEs from NKJP
  , noDates   :: Bool
    -- ^ Discard NKJP dates
  , onlySure  :: Bool
    -- ^ Discard wrongly annotated MWEs and MWEs with unknown reading
  } deriving (Show)


-- | By default all types of MWEs allowed.
defaultSelectCfg :: SelectCfg
defaultSelectCfg = SelectCfg
  { noWalenty = True
  , noSejf = True
  , noNKJP = True
  , noDates = True
  , onlySure = False }


-- | Compile a predicate which selects MWEs consistent
-- with the given configuration.
compileSelect :: SelectCfg -> MWE.MweInfo -> Bool
compileSelect SelectCfg{..} MWE.MweInfo{..} =
  not discard
  where
    discard = or
      [ noWalenty && origin == Just "walenty"
      , noSejf && origin == Just "sejf"
      , noNKJP && origin == Just "nkjp"
      , noDates && mweTyp == Just "date"
      , onlySure &&
        (  reading == Just MWE.Error
        || reading == Nothing ) ]


------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------


-- | Print, in four columns, statistics about the hypergraph size.
printHypeStats :: AStar.Hype Text Text -> IO ()
printHypeStats hype = do
  putStr ","
  putStr . show $ AStar.doneNodesNum hype
  putStr ","
  putStr . show $ AStar.doneEdgesNum hype
  putStr ","
  putStr . show $ AStar.waitingNodesNum hype
  putStr ","
  putStr . show $ AStar.waitingEdgesNum hype
