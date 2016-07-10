{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}


-- | A module responsible for identifying occurrences of the
-- Walenty valency dictionary entries in the Składnica treebank.


module NLP.Skladnica.Extract
( extractGrammar
) where


import           Control.Monad                 (forM_, when)
import           Control.Monad.IO.Class        (liftIO)
import qualified Control.Monad.State.Strict    as E

-- import qualified Data.ByteString               as BS
import           Data.Either                   (lefts)
import qualified Data.Foldable                 as F
import qualified Data.Traversable              as Trav
import           Data.List                     (minimumBy)
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (mapMaybe)
import qualified Data.MemoCombinators          as Memo
import           Data.Ord                      (comparing)
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Tree                     as R

import qualified System.FilePath.Find          as F

import qualified NLP.Partage.AStar             as AStar
import qualified NLP.Partage.AStar.DepTree     as Dep
import qualified NLP.Partage.AStar.Deriv       as Deriv
import qualified NLP.Partage.AStar.Deriv.Gorn  as Gorn
import qualified NLP.Partage.DAG               as DAG

import qualified NLP.Skladnica                 as Skl
import qualified NLP.Walenty                   as W

import qualified NLP.Skladnica.Walenty.Grammar as G
import qualified NLP.Skladnica.Walenty.MweTree as MWE
import qualified NLP.Skladnica.Walenty.Search  as Search


------------------------------------------------------------------------------
-- Grammar Extraction Tests
--------------------------------------------------------------------------------


-- | Składnica tree.
type SklTree = Skl.Tree Skl.Node Skl.IsHead


-- | Derivation tree.
type DerTree = Deriv.Deriv T.Text T.Text


-- | Dependency tree (for Składica trees)
type DepTree = Dep.Tree (S.Set (AStar.Tok T.Text)) ()


-- | Extraction result.
data Extract = Extract
  { gramSet :: S.Set G.ET
    -- ^ The resulting grammar
  , dataSet :: M.Map SklTree DerTree
    -- ^ Derivation trees assigned to Składnica trees
  } deriving (Show, Eq, Ord)


emptyExtract :: Extract
emptyExtract = Extract S.empty M.empty


-- showExtract :: Extract -> IO ()
-- showExtract Extract{..} = do
--   putStr "PARSED FILES: " >> print (S.size parsedFiles)
--   putStr "SEEN FILES: " >> print (S.size seenFiles)
--   putStr "MWE FILES: " >> print (S.size mweFiles)
--   putStr "GRAM TREES: " >> print (S.size gramSet)
--   putStr "FREQ MAP: " >> print (M.size freqMap)


-- -- | Read all verb entries from Walenty and search for them
-- -- in Składnica treebank.
-- -- Extract the grammar from the resulting syntactic trees
-- -- (both with and without MWEs).
-- extractGrammar'
--   :: FilePath -- ^ Składnica XML file
--   -> IO Extract
-- extractGrammar' skladnicaXML = do
--   trees <- MWE.readTop skladnicaXML
--   flip E.execStateT emptyExtract $ do
--     forM_ trees $ \sklTree0 -> do
--       let mweTree = fmap MWE.sklNode (MWE.emboss sklTree0)
--           sklTree = fmap MWE.sklNode sklTree0
--           sklETs = G.extractGrammar sklTree
--           mweETs = G.extractGrammar mweTree
--           est = sklETs `S.union` mweETs
--           realMweETs = mweETs `S.difference` sklETs
--       when (S.null est) $ do
--         liftIO $ putStrLn "WARNING: something went wrong..."
--       when (not $ S.null realMweETs) $ do
--         -- putStrLn "MWE elementary trees found:\n"
--         liftIO $ putStrLn
--           . R.drawForest . map (fmap show)
--           . S.toList $ realMweETs


-- | Extract local grammars from the individual sentences in the input forest,
-- parse with the local grammars and show the corresponding derivation and
-- dependency trees.
extractGrammar
  :: FilePath -- ^ Składnica XML file
  -> String   -- ^ Start symbol
  -> IO Extract
extractGrammar skladnicaXML begSym0 = do
  trees <- MWE.readTop skladnicaXML
  flip E.execStateT emptyExtract $ do
    forM_ trees $ \sklTree0 -> do
      let mweTree = fmap MWE.sklNode (MWE.emboss sklTree0)
          sklTree = fmap MWE.sklNode sklTree0
          sklETs = G.extractGrammar sklTree
          mweETs = G.extractGrammar mweTree
          est = sklETs `S.union` mweETs
      when (S.null est) $ do
        liftIO $ putStrLn "WARNING: something went wrong..."
      let begSym = T.pack begSym0
          gram = buildGram est
          termMemo = Memo.wrap read show $ Memo.list Memo.char
          auto = AStar.mkAuto termMemo gram
          sent = baseForms sklTree
          sentLen = length sent
          input = AStar.fromList sent
          putRose = putStrLn . R.drawTree . fmap show
      liftIO $ do
        T.putStrLn $ "# PARSING: " `T.append` T.unwords (orthForms sklTree)
        putStrLn $ "# SKLADNICA DEPENDENCY TREE:\n"
      -- reference dependency tree
      let depTree0 = asDepTree . tokenize $ sklTree
      liftIO $ putRose . Dep.toRose $ depTree0
      -- computing the set of derivations
      hype <- liftIO $ AStar.earleyAuto auto input
      let derivList = Deriv.derivTrees hype begSym sentLen
          derivSize = Gorn.size . Gorn.fromDeriv
          derivTree = minimumBy (comparing derivSize) derivList
          depTree = Dep.fromDeriv . Gorn.fromDeriv $ derivTree
      liftIO $ do
        putRose . Deriv.deriv4show $ derivTree
        putRose . Dep.toRose $ depTree



------------------------------------------------------------------------------
-- Składnica Dependency Tree
--------------------------------------------------------------------------------


-- | Składnica tree where an identifier is added to each node, which
-- is supposed to represent the positions of the leaf nodes.
-- IDs assigned to internal nodes is irrelevant.
type TokTree = Skl.Tree (Skl.Node, Int) Skl.IsHead


-- | Assign positions to the individual leaf nodes.
tokenize :: SklTree -> TokTree
tokenize =
  snd . Trav.mapAccumL f (0 :: Int)
  where
    f acc x = case Skl.label (Skl.nodeLabel x) of
      Left _nonTerm -> (acc,   Skl.modifyNode (,0)   x)
      Right _term   -> (acc+1, Skl.modifyNode (,acc) x)


-- | Extract the dependency tree consistent with the given Składnica tree.
asDepTree :: TokTree -> DepTree
asDepTree root@R.Node{..} = Dep.Tree
  { Dep.root = anchors root
  , Dep.children = M.fromList
    [ (asDepTree subTree, ())
    | subTree <- subForest
    , Skl.edgeLabel (R.rootLabel subTree) == Skl.HeadNo ]
  }


-- asDepTree' :: TokTree -> DepTree
-- asDepTree' root@R.Node{..} =
--   join heads `addChildren` others
--   where
--     children =
--       [ ( asDepTree' subTree
--         , Skl.edgeLabel (R.rootLabel subTree) )
--       | subTree <- subForest ]
--     heads  = filter ((==Skl.HeadYes) . snd) children
--     others = filter ((==Skl.HeadNo)  . snd) children
--     join ts = Dep.Tree
--       { Dep.root = S.unions (map Dep.root ts)
--       , Dep.children = M.unions (map Dep.children ts) }
--     addChildren root ts = root
--       { Dep.children = M.union
--         (Dep.children root)
--         (M.fromList [(t, ()) | t <- ts]) }


-- | Find anchoring terminals for the given Składnica node.
anchors :: TokTree -> S.Set (AStar.Tok T.Text)
anchors R.Node{..} =
  here `S.union` below
  where
    nodeLabel = Skl.nodeLabel rootLabel
    here = case Skl.label (fst nodeLabel) of
      Left _nonTerm -> S.empty
      Right term    -> S.singleton $ AStar.Tok
        { AStar.position = snd nodeLabel
        -- WARNING: below, note that we parse over base forms
        , AStar.terminal = Skl.base term }
    below = S.unions
      [ anchors subTree | subTree <- subForest
      , Skl.edgeLabel (R.rootLabel subTree) == Skl.HeadYes ]


------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------


-- | Build a grammar from the given set of ETs.
buildGram :: S.Set G.ET -> DAG.Gram T.Text T.Text
buildGram = DAG.mkGram . map (,1) . S.toList


-- | Retrieve terminal base forms from the given syntactic tree.
baseForms :: SklTree -> [T.Text]
baseForms = map Skl.base . Search.terminals


-- | Retrieve terminal orth forms from the given syntactic tree.
orthForms :: SklTree -> [T.Text]
orthForms = map Skl.orth . Search.terminals
