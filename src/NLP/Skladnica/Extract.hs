{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}


-- | A module responsible for identifying occurrences of the
-- Walenty valency dictionary entries in the Składnica treebank.


module NLP.Skladnica.Extract
(
-- * Types
  SklTree
, DerTree
, DepTree
, Tok
, derivSize

-- * Extraction
, Extract (..)
, extractGrammar
, fromFile
, parse
, parsePipe
, findDeriv

, TermType (..)
, wordForms

-- * Utils
, buildGram
, buildFreqGram
) where


import           Control.Monad                 (forM_, when, guard)
import           Control.Monad.IO.Class        (liftIO)
import qualified Control.Monad.State.Strict    as E

-- import qualified Data.ByteString               as BS
import           Data.Either                   (lefts)
import qualified Data.Foldable                 as F
import qualified Data.Traversable              as Trav
import           Data.List                     (minimumBy, partition)
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (mapMaybe, isJust)
import qualified Data.MemoCombinators          as Memo
import           Data.Ord                      (comparing)
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Tree                     as R

import qualified System.FilePath.Find          as F

import           Pipes

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


-- | Token of a derivation/dependency tree
type Tok = AStar.Tok T.Text


-- | Derivation tree.
type DerTree = Deriv.Deriv T.Text Tok


-- | Dependency tree (for Składica trees)
type DepTree = Dep.Tree (S.Set Tok) ()


-- | Return the size (i.e., the number of ndoes)of the derivation tree.
derivSize :: DerTree -> Int
derivSize = Gorn.size . Gorn.fromDeriv


-- | Extracted grammar.
data Extract = Extract
  { gramSet :: G.TAG
  , freqMap :: M.Map T.Text Int
  } deriving (Show, Eq, Ord)


emptyExtract :: Extract
emptyExtract = Extract G.empty M.empty


-- | Extract TAG grammar from the input XML treebank.
fromFile
  :: TermType
  -> (MWE.MweInfo -> Bool)
  -> FilePath
  -> IO Extract
fromFile termTyp mwePred skladnicaXML = do
  trees <- MWE.readTop skladnicaXML
  flip E.execStateT emptyExtract $ do
    forM_ trees $ \sklTree0 -> do
      -- let mweTree = fmap MWE.sklNode (MWE.embossTop mwePred sklTree0)
      let mweTree = MWE.modifyRoot (fmap MWE.sklNode . MWE.emboss mwePred) sklTree0
          sklTree = MWE.modifyRoot (fmap MWE.sklNode) sklTree0
          sklETs = G.extractGrammar $ MWE.topRoot sklTree
          mweETs = G.extractGrammar $ MWE.topRoot mweTree
          est = sklETs `S.union` mweETs
      when (S.null est) $ do
        liftIO $ putStrLn "WARNING: something went wrong..."
      E.modify' $ \st ->
        let newFreqMap = freqMapFrom termTyp $ MWE.topRoot sklTree
            force = seq . length . show
        in  force (est, newFreqMap) $ st
            { gramSet = S.union (gramSet st) est
            , freqMap = M.unionWith (+) (freqMap st) newFreqMap }


-- | Parse the given sentence with the given grammar.
parse
  :: [T.Text] -- ^ Input sentence
  -> T.Text   -- ^ Start symbol
  -> G.TAG    -- ^ Grammar
  -> IO [DerTree]
parse sent begSym tag = do
  let gram = buildGram tag
      termMemo = Memo.wrap read show $ Memo.list Memo.char
      auto = AStar.mkAuto termMemo gram
      sentLen = length sent
      input = AStar.fromList sent
  hype <- AStar.earleyAuto auto input
  return $ Deriv.derivTrees hype begSym sentLen


-- | Parse the given sentence with the given compressed grammar.
parsePipe
  :: [T.Text] -- ^ Input sentence
  -> T.Text   -- ^ Start symbol
  -> DAG.Gram T.Text T.Text -- ^ Compressed TAG grammar
  -> Producer (Deriv.ModifDerivs T.Text T.Text) IO (AStar.Hype T.Text T.Text)
parsePipe sent begSym gram =
  let termMemo = Memo.wrap read show $ Memo.list Memo.char
      auto = AStar.mkAuto termMemo gram
      sentLen = length sent
      input = AStar.fromList sent
      derivConf = Deriv.DerivR
        { Deriv.startSym = begSym
        , Deriv.sentLen = sentLen }
  in  AStar.earleyAutoP auto input
      >-> Deriv.derivsPipe derivConf
--       -- below we make the AStar pipe work in the State monad
--       -- transformer over IO (normally it works just over IO)
--       pipe = Morph.hoist E.lift
--         ( AStar.earleyAutoP auto input
--           >-> Deriv.derivsPipe derivConf )


-- | Find the minimal derivation consistent with the given Składnica
-- syntactic tree.
miniDerivOf :: [DerTree] -> SklTree -> Maybe DerTree
miniDerivOf derivList sklTree =
  let depTree = canonize . asDepTree . tokenize $ sklTree
      derivSize = Gorn.size . Gorn.fromDeriv
      minimumBy' cmp xs = case xs of
        [] -> Nothing
        _  -> Just $ minimumBy cmp xs
  in  minimumBy' (comparing derivSize)
        . filter ( (==depTree) . canonize . Dep.mapDep (const ())
               . Dep.fromDeriv . Gorn.fromDeriv )
        $ derivList


-- -- | Extract TAG corresponding to the given Składnica tree.
-- localTAG :: MWE.MweTree -> G.TAG
-- localTAG sklTree0 =
--   let mweTree = fmap MWE.sklNode (MWE.emboss sklTree0)
--       sklTree = fmap MWE.sklNode sklTree0
--       sklETs = G.extractGrammar sklTree
--       mweETs = G.extractGrammar mweTree
--   in  sklETs `S.union` mweETs


-- -- | Find a derivation tree corresponding to the given MWE-marked tree. We
-- -- assume that the optimal derivation contains the highest number of MWEs.
-- findDeriv
--   :: Int          -- ^ Upper limit on the number of generated derivations
--   -> T.Text       -- ^ Grammar start symbol (TODO: could be deduced?)
--   -> TermType
--   -> MWE.MweTree  -- ^ MWE-marked Składnica tree
--   -> IO (Maybe DerTree)
-- findDeriv maxNum begSym termTyp sklTree0 = do
--   let mweTree = fmap MWE.sklNode (MWE.emboss sklTree0)
--       sklTree = fmap MWE.sklNode sklTree0
--       sklETs = G.extractGrammar sklTree
--       mweETs = G.extractGrammar mweTree
--       est = sklETs `S.union` mweETs
--   derivList <- take maxNum <$> parse (wordForms termTyp sklTree) begSym est
--   return $ miniDerivOf derivList mweTree


-- | Find a derivation tree corresponding to the given Składnica tree
findDeriv
  :: Int          -- ^ Upper limit on the number of generated derivations
  -> T.Text       -- ^ Grammar start symbol (TODO: could be deduced?)
  -> TermType     -- ^ Type of terminals involved
  -> SklTree      -- ^ Składnica tree
  -> IO (Maybe DerTree)
findDeriv maxNum begSym termTyp sklTree = do
  let ets = G.extractGrammar sklTree
  derivList <- take maxNum <$> parse (wordForms termTyp sklTree) begSym ets
  return $ miniDerivOf derivList sklTree

-- | Extract local grammars from the individual sentences in the input forest,
-- parse with the local grammars and show the corresponding derivation and
-- dependency trees.
extractGrammar
  :: FilePath -- ^ Składnica XML file
  -> String   -- ^ Start symbol
  -> (MWE.MweInfo -> Bool)
  -> IO Extract
extractGrammar skladnicaXML begSym0 mwePred = do
  trees <- MWE.readTop skladnicaXML
  flip E.execStateT emptyExtract $ do
    forM_ trees $ \sklTree0 -> do
      let mweTree = MWE.modifyRoot (fmap MWE.sklNode . MWE.emboss mwePred) sklTree0
          sklTree = MWE.modifyRoot (fmap MWE.sklNode) sklTree0
          sklETs = G.extractGrammar $ MWE.topRoot sklTree
          mweETs = G.extractGrammar $ MWE.topRoot mweTree
          est = sklETs `S.union` mweETs
      liftIO $ do
        when (S.null est) $ putStrLn "ERROR: something went wrong..."
        putStrLn $ "===========================================\n"
        T.putStrLn $ "# PARSING: " `T.append`
          T.unwords (_orthForms $ MWE.topRoot sklTree)
        putStrLn $ "# SKLADNICA DEPENDENCY TREE:\n"

      -- reference dependency tree
      let putRose = putStrLn . R.drawTree . fmap show
          refTree = canonize . asDepTree . tokenize . MWE.topRoot $ mweTree
      liftIO $ mapM_ (putRose . Dep.toRose) (S.toList refTree)

      -- computing the set of derivations
      let maxLength = 1000000 -- TODO: make it a parameter
      derivList <- liftIO $ take maxLength
        <$> parse (_baseForms $ MWE.topRoot sklTree) (T.pack begSym0) est
      let derivNum = length derivList
      liftIO . putStrLn $
        "# NO. OF DERIVATIONS: " ++ show derivNum ++
        if derivNum == maxLength then "+" else ""

      let derivTreeMay = miniDerivOf derivList $ MWE.topRoot mweTree
      case derivTreeMay of
        Nothing -> liftIO $ do
          putStrLn "# THE CORRESPONDING DERIVATION NOT FOUND"
          putStrLn "# BELOW, SOME OF THE POSSIBLE DERIVATIONS:\n"
          forM_ (take 10 derivList) $ \derivTree -> do
            let depTree = Dep.fromDeriv . Gorn.fromDeriv $ derivTree
            liftIO $ do
              putRose . Deriv.deriv4show $ derivTree
              putRose . Dep.toRose $ depTree
              mapM_
                (putRose . Dep.toRose)
                (S.toList . canonize . Dep.mapDep (const ()) $ depTree)
        Just derivTree -> do
          let depTree = Dep.fromDeriv . Gorn.fromDeriv$  derivTree
          liftIO $ do
            putStrLn "# THE CORRESPONDING DERIVATION:\n"
            putRose . Deriv.deriv4show $ derivTree
            putRose . Dep.toRose $ depTree
            -- putRose . Dep.toRose . canonize . Dep.mapDep (const ()) $ depTree


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


-- -- | Extract the dependency tree consistent with the given Składnica tree.
-- asDepTree :: TokTree -> DepTree
-- asDepTree R.Node{..} =
--   join heads `addChildren` others
--   where
--     children =
--       [ ( asDepTree subTree
--         , Skl.edgeLabel (R.rootLabel subTree) )
--       | subTree <- subForest ]
--     heads  = [t | (t, Skl.HeadYes) <- children]
--     others = [t | (t, Skl.HeadNo)  <- children]
--     join ts = Dep.Tree
--       { Dep.root = S.unions (termsHere : map Dep.root ts)
--       , Dep.children = M.unions (map Dep.children ts) }
--     addChildren root ts = root
--       { Dep.children = M.union
--         (Dep.children root)
--         (M.fromList [(t, ()) | t <- ts]) }
--     nodeLabel = Skl.nodeLabel rootLabel
--     termsHere = case Skl.label (fst nodeLabel) of
--       Left _nonTerm -> S.empty
--       Right term    -> S.singleton $ AStar.Tok
--         { AStar.position = snd nodeLabel
--         -- WARNING: below, note that we parse over base forms
--         , AStar.terminal = Skl.base term }


-- | Extract the dependency tree consistent with the given Składnica tree.
asDepTree :: TokTree -> DepTree
asDepTree R.Node{..} =
  join heads `addChildren` others
  where
    (heads0, others0) = partition isHead subForest
    heads  = map fst $ asDeps heads0
    others = map fst $ asDeps others0
    asDeps ts = [(asDepTree t, Skl.edgeLabel (R.rootLabel t)) | t <- ts]
    join ts = Dep.Tree
      { Dep.root = S.unions (termsHere : map Dep.root ts)
      , Dep.children = M.unions (map Dep.children ts) }
    addChildren root ts = root
      { Dep.children = M.union
        (Dep.children root)
        (M.fromList [(t, ()) | t <- ts]) }
    nodeLabel = Skl.nodeLabel rootLabel
    termsHere = case Skl.label (fst nodeLabel) of
      Left _nonTerm -> S.empty
      Right term    -> S.singleton $ AStar.Tok
        { AStar.position = snd nodeLabel
        -- WARNING: below, note that we parse over base forms
        , AStar.terminal = Skl.base term }

    -- When a node should be considered as a head?
    -- Note that a terminal should be always considered as a head, while it
    -- is not always marked as such in Składnica.
    isHead t = headYes t || fweTerm t || isTerm t
    -- Is it explicitely marked as a head node?
    headYes t = Skl.edgeLabel (R.rootLabel t) == Skl.HeadYes
    -- Or maybe it is an "fw" node with a terminal child?
    fweTerm fw = isJust $ do
      fwCat <- nonTermCat fw
      guard $ fwCat == "fw"
      term <- child fw
      guard $ isTerm term
    child t = case R.subForest t of
      [c] -> Just c
      _ -> Nothing
    nonTermCat t = case Skl.label . fst . Skl.nodeLabel . R.rootLabel $ t of
      Left Skl.NonTerm{..} -> Just cat
      _ -> Nothing
    isTerm t = case Skl.label . fst . Skl.nodeLabel . R.rootLabel $ t of
        Right _term -> True
        _ -> False


-- prepare :: SklTree -> SklTree
-- prepare =
--   check . Skl.purge useless
--   where
--     check [x] = x
--     check xs  = error $ "prepTree: purge left " ++ show xs
--     useless Skl.Edge{..} = case Skl.label nodeLabel of
--       (Left Skl.NonTerm{..}) -> cat `elem` ["fw", "fl", "ff"]
--       _ -> False


-- | Construct a canonical form of a dependency tree.
canonize :: DepTree -> S.Set DepTree
canonize =
  S.fromList . Dep.discard useless
  where
    useless = all (isInterp . AStar.terminal) . S.toList
    isInterp = flip S.member interps
    interps = S.fromList [","]


-- -- | Extract the dependency tree consistent with the given Składnica tree.
-- asDepTree :: TokTree -> DepTree
-- asDepTree root@R.Node{..} = Dep.Tree
--   { Dep.root = anchors root
--   , Dep.children = M.fromList
--     [ (asDepTree subTree, ())
--     | subTree <- subForest
--     , Skl.edgeLabel (R.rootLabel subTree) == Skl.HeadNo ]
--   }
--
--
-- -- | Find anchoring terminals for the given Składnica node.
-- anchors :: TokTree -> S.Set (AStar.Tok T.Text)
-- anchors R.Node{..} =
--   here `S.union` below
--   where
--     nodeLabel = Skl.nodeLabel rootLabel
--     here = case Skl.label (fst nodeLabel) of
--       Left _nonTerm -> S.empty
--       Right term    -> S.singleton $ AStar.Tok
--         { AStar.position = snd nodeLabel
--         -- WARNING: below, note that we parse over base forms
--         , AStar.terminal = Skl.base term }
--     below = S.unions
--       [ anchors subTree | subTree <- subForest
--       , Skl.edgeLabel (R.rootLabel subTree) == Skl.HeadYes ]


------------------------------------------------------------------------------
-- Extracting Word Forms
--------------------------------------------------------------------------------


-- | Type of terminals to use for the experiments.
data TermType = Orth | Base
  deriving (Show, Read, Eq, Ord)


-- | Retrieve terminal orth forms from the given syntactic tree.
wordForms :: TermType -> SklTree -> [T.Text]
wordForms tt = case tt of
  Orth -> _orthForms
  Base -> _baseForms


-- | Retrieve terminal base forms from the given syntactic tree.
_baseForms :: SklTree -> [T.Text]
_baseForms = map Skl.base . Search.terminals


-- | Retrieve terminal orth forms from the given syntactic tree.
_orthForms :: SklTree -> [T.Text]
_orthForms = map Skl.orth . Search.terminals


------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------


-- | Build a grammar from the given set of ETs.
buildGram :: S.Set G.ET -> DAG.Gram T.Text T.Text
buildGram = DAG.mkGram . map (,1) . S.toList


-- | Build a grammar from the given set of ETs.
buildFreqGram
  :: M.Map T.Text Int     -- ^ Frequency map
  -> S.Set G.ET
  -> DAG.Gram T.Text T.Text
buildFreqGram fm = DAG.mkFreqGram fm . map (,1) . S.toList


-- | Extract frequency map from a tree.
freqMapFrom
  :: TermType
  -> SklTree
  -> M.Map T.Text Int
freqMapFrom tt = count . wordForms tt


-- | Count elements in the input list.
count :: Ord a => [a] -> M.Map a Int
count = M.fromListWith (+) . map (,1)
