{-# LANGUAGE RecordWildCards #-}


module NLP.Skladnica.New
( GlobalCfg (..)
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
import qualified Data.Tree                     as R

import           Pipes

import qualified NLP.Partage.AStar             as AStar

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
--   , maxDerivNum     :: Int
--     -- ^ Maximum number of derivation to generate to find the
--     -- derivation corresponding to the given Składnica tree
  , termTyp         :: Ext.TermType
    -- ^ What type of terminals use in the experiments
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

  putStrLn "\n===== GRAMMAR EXTRACTION =====\n"
  extract <- Ext.fromFile termTyp skladnicaXML

  putStrLn "\n===== EXTRACTED GRAMMAR =====\n"
  forM_ (S.toList $ Ext.gramSet extract) $
    putStrLn . R.drawTree . fmap show

  -- how to build a grammar?
  let buildGram = if useFreqs
                  then Ext.buildFreqGram (Ext.freqMap extract)
                  else Ext.buildGram

  -- single global grammar for all
  let globGram = buildGram (Ext.gramSet extract)

  putStrLn "\n===== PARSING TESTS =====\n"
  skladnica <- MWE.readTop skladnicaXML

  putStr "length,"
  putStr "chart-nodes-1,chart-arcs-1,agenda-nodes-1,agenda-arcs-1,"
  putStr "chart-nodes-2,chart-arcs-2,agenda-nodes-2,agenda-arcs-2"
  putStrLn ""

  -- flip E.execStateT () $ forM_ skladnica $ \sklTree0 -> do
  forM_ skladnica $ \sklTree0 -> do

    -- First we construct two versions of the syntactic tree: one compositional,
    -- one which assumes MWE interpretations.
    let sklTree = fmap MWE.sklNode sklTree0
        mweTree = fmap MWE.sklNode (MWE.emboss sklTree0)
    -- Stop if the two versions are identical.
    -- guard $ mweTree /= sklTree
    if mweTree == sklTree
      then return ()
      else do

        -- Some utility "variables"
        let sent = Ext.wordForms termTyp sklTree
            sentLen = length sent
            final p = AStar._spanP p == AStar.Span 0 sentLen Nothing
                   && AStar._dagID p == Left begSym
            getWeight e = AStar.priWeight e + AStar.estWeight e

        -- Column 1: sentence length
        putStr (show sentLen)

        -- Build the local grammar (simple form of super-tagging)
        let localETs = Select.select (S.fromList sent) (Ext.gramSet extract)
            localGram = if restrictGrammar then buildGram localETs else globGram

        -- Used to control the state of the parsing process
        contRef <- liftIO $ newIORef None

        -- let pipe = Morph.hoist E.lift Ext.parsePipe
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
                  -- Columns 2,3,4,5: hype stats at checkpoint 1
                  printHypeStats hype
              Done -> return ()
        -- Columns 6,7,8,9: hype stats at the end
        printHypeStats hypeFini
        putStrLn ""

--     derivRegMay <- Ext.findDeriv maxDerivNum begSym mweTree
--     derivMweMay <- Ext.findDeriv maxDerivNum begSym mweTree
--     case derivMay of
--       Nothing -> return ()
--       Just _deriv -> do
--         -- now we have the corresponding reference derivation



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
