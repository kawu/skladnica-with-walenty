{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


-- | A module responsible for identifying occurrences of the
-- Walenty valency dictionary entries in the Skladnica treebank.


module NLP.Skladnica.Walenty.Mapping
(
-- * Types and Funs
  InTree
, OutTree
, Edge
, terminals
, querify
, markSklTree

-- * Utilities
, trunk
, hasBase
, hasTag
, hasOrth
) where


import           Control.Applicative           (empty, (<|>))
import           Control.Monad                 (guard)
import qualified Control.Monad.State.Strict    as E
import           Control.Monad.Trans.Maybe     (MaybeT (..), mapMaybeT)

-- import           Data.Foldable             (foldMap)
import           Data.List                     (foldl')
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (catMaybes, fromMaybe, isJust)
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Tree                     as R
-- import qualified Data.Text.Lazy            as L

import qualified NLP.Skladnica                 as S
import qualified NLP.Walenty.Types             as W

import qualified NLP.Skladnica.Walenty.Search2 as Q


-- | `Edge` from Składnica (or rather, node with info about the ingoing edge).
type Edge = S.Edge S.Node S.IsHead


-- | Morphosyntactic attribute.
type Attr = Text
type AttrVal = Text


-- | Skladnica input tree.
type InTree = R.Tree (S.Edge S.Node S.IsHead)


-- | Skladnica output (i.e., with MWEs) tree.
-- type OutTree = S.Tree (S.Node, Set.Set i) S.IsHead
type OutTree = R.Tree (S.Edge S.Node S.IsHead, Set.Set S.NID)


-- | Retrieve terminal leaves of the given tree.
terminals
  :: InTree
  -> [S.Term]
terminals =
  let getTerm S.Node{..} = case label of
        Left _  -> []
        Right t -> [t]
  in foldMap getTerm . S.simplify


-- | Mark Składnica tree with MWEs.
markSklTree :: [Q.Expr Edge 'Q.Tree] -> InTree -> OutTree
markSklTree = Q.markAll (S.nid . S.nodeLabel)


-- | Convert the given verbal entry from Walenty to a query.
--
-- TODO: The following are ignored for the moment:
--
--   * Negativity
--   * Predicativity
--   * ...
--
querify :: W.Verb -> Q.Expr Edge Q.Tree
querify verb = Q.andQ
  [ trunk . hasBase . W.base $ verb
  , frameQ (W.frame verb) ]


-- | A query expression for a frame.
--
-- TODO: At the moment it is not enfornced that each argument
-- is realized by a different tree child node!
frameQ :: W.Frame -> Q.Expr Edge Q.Tree
-- frameQ frame = andQ $ map (Child . Child . argumentQ) frame
frameQ frame = Q.andQ $ map (Q.anyChild . metaArgQ) frame


-- | Pseudo-frame in which one or more of the arguments must be realized.
-- Useful within the context of constraints on dependents, which are
-- called "frames" but are not really.
--
-- TODO: Difficult choice, sometimes it seems that all dependents specified
-- in `RAtr` should be present, sometimes that only some of them...
pseudoFrameQ :: W.Frame -> Q.Expr Edge Q.Tree
-- pseudoFrameQ frame = orQ $ map (Child . metaArgQ) frame
pseudoFrameQ = frameQ


-- | Handle (and ignore) nodes explicitely marked
-- with "fw", "fl", "ff".
metaArgQ :: W.Argument -> Q.Expr Edge Q.Tree
metaArgQ arg =
  Q.ifThenElse
    isMetaNode
    -- (skipAnyChild argument)
    (Q.anyChild argument)
    argument
  where
    argument = argumentQ arg
    isMetaNode = Q.Current . isNonTerm $
      \S.NonTerm{..} -> cat `elem` ["fw", "fl", "ff"]


-- | A query expression for an argument.
-- TODO: function ignored.
argumentQ :: W.Argument -> Q.Expr Edge Q.Tree
argumentQ arg = Q.orQ . map phraseQ $ W.phraseAlt arg


phraseQ :: W.Phrase -> Q.Expr Edge Q.Tree
phraseQ p = case p of
  W.Standard s -> stdPhraseQ s
  W.Special s -> specPhraseQ s


stdPhraseQ :: W.StdPhrase -> Q.Expr Edge Q.Tree
stdPhraseQ phrase = case phrase of
  W.NP{..} -> Q.andQ
    [ Q.Current $ Q.andQ
      [ hasCat "fno"
      , caseQ caseG
      , Q.maybeQ agrNumber agreeNumQ ]
    , Q.andQ
      [ lexicalQ lexicalHead
      , dependentsQ dependents ]
    ]
  W.PrepNP{..} -> Q.andQ
    [ Q.Current $ Q.andQ
      [ hasCat "fpm"
      , hasAttr przyimek $ preposition
      , hasAttr klasa "rzecz"
      , caseQ caseG ]
    -- we "skip" the child so that potential agreement works
    -- between the parent of PP and the NP (not sure if this
    -- is necessary)
    -- , skipAnyChild $ andQ
    , Q.anyChild $ Q.andQ
        [ Q.Current $ hasCat "fno"
        , Q.Current $ Q.maybeQ agrNumber agreeNumQ
        , lexicalQ lexicalHead
        , dependentsQ dependents
        ]
    ]
  -- Don't know how to handle this yet, and it is not handled
  -- by the default handler below (which referes to dependents)
  W.ComparP{} -> Q.B False
  -- By default we check if (a) lexical requirements are satisfied for the
  -- argument itself, directly, or (b) for one of its children, which makes
  -- sense for certain phrase types (e.g., `CP`)
  p ->
    let checkLex = Q.andQ
          [ lexicalQ (W.lexicalHead p)
          , dependentsQ (W.dependents p) ]
    -- in  checkLex `Or` skipAnyChild checkLex
    in  checkLex `Q.Or` Q.anyChild checkLex

--   W.CP{..} -> andQ
--     [ Current $ andQ
--       [ hasCat "fzd" ]
--     , anyChild $ andQ
--       [ lexicalQ lexicalHead
--       , dependentsQ dependents ]
--     ]


specPhraseQ :: W.SpecPhrase -> Q.Expr Edge Q.Tree
specPhraseQ p = case p of
  W.XP{..} -> Q.maybeQ xpVal phraseQ
  -- TODO: not handled yet
  W.Fixed{} -> Q.B False
  _ -> Q.B True
-- specPhraseQ _ = B True


-- | Constraints on lexical heads.
lexicalQ :: [Text] -> Q.Expr Edge Q.Tree
lexicalQ xs = if null xs
  then Q.B True
  else trunk (hasBases xs)


-- | Follow the trunk!
trunk :: Q.Expr Edge Q.Node -> Q.Expr Edge Q.Tree
trunk = Q.ancestor $ \x -> S.edgeLabel x == S.HeadYes


-- | Constraints stemming from the requirements over the dependents.
dependentsQ :: W.Attribute -> Q.Expr Edge Q.Tree
dependentsQ deps = case deps of
  -- no modifiers allowed
  W.NAtr -> Q.NonBranching
  -- modifiers allowed but optional; TODO: we could check that all modifiers
  -- present are consistent with the given `Atr` list.
  W.Atr _ -> Q.B True
  -- TODO: not distinguished from `Atr` case.
  W.Atr1 _ -> Q.B True
  -- at least one of the attributes given in the list must be present
  W.RAtr xs -> pseudoFrameQ xs
  -- TODO: we should check that there is at most one modifier.
  W.RAtr1 xs -> pseudoFrameQ xs
  _ -> Q.B True


-- | Skladnica case value based on the Walenty case value.
caseQ :: W.Case -> Q.Expr Edge Q.Node
caseQ c =
  pr $ case c of
    W.Nominative -> sg "mian"
    W.Genitive -> sg "dop"
    W.Dative -> sg "cel"
    W.Accusative -> sg "bier"
    W.Instrumental -> sg "narz"
    W.Locative -> sg "miej"
    W.Partitive -> ["dop", "bier"]
    -- TODO: structural case should depend on the function, can be
    -- precomputed at the compilation stage.
    W.Structural -> ["mian", "dop", "bier"]
    -- TODO: not sure if can be resolved at the compilation stage
    W.Agreement -> []
    W.PostPrep -> sg "pop"
    -- TODO: not handled by Agata's specification
    W.Predicative -> []
    where
      sg x = [x]
      pr xs = if null xs
        then Q.B True
        else Q.orQ $ map (hasAttr przypadek) xs


agreeNumQ :: W.Agree W.Number -> Q.Expr Edge Q.Node
agreeNumQ agreeNum = case agreeNum of
  W.Value v -> hasAttr liczba $ liczbaSKL v
--   W.Agree -> Satisfy2 $ \parent child -> isJust $ do
--     x <- getAttr liczba parent
--     y <- getAttr liczba child
--     guard $ x == y
  _ -> Q.B True


-- | Check if the node is a terminal node with the given base.
hasBase :: Text -> Q.Expr Edge Q.Node
hasBase x = hasBases [x]


-- | Check if the node is a terminal node with one of the given orth values.
hasOrth :: (Text -> Bool) -> Q.Expr Edge Q.Node
hasOrth p0 =
  let p S.Term{..} = p0 orth
  in  isTerm p `Q.And` Q.Mark


-- | Check if the node is a terminal node with one of the given base values.
hasBases :: [Text] -> Q.Expr Edge Q.Node
hasBases xs =
  let p S.Term{..} = base `elem` xs
  in isTerm p `Q.And` Q.Mark


-- | Check if the node is a terminal node with the given tag value.
hasTag :: Text -> Q.Expr Edge Q.Node
hasTag x = isTerm $ \S.Term{..} -> tag == x


isNonTerm :: (S.NonTerm -> Bool) -> Q.Expr Edge Q.Node
isNonTerm p = Q.SatisfyNode $ \S.Edge{..} -> isJust $ do
  nonTerm@S.NonTerm{..} <- takeLeft (S.label nodeLabel)
  guard $ p nonTerm


isTerm :: (S.Term -> Bool) -> Q.Expr Edge Q.Node
isTerm p = Q.SatisfyNode $ \S.Edge{..} -> isJust $ do
  term@S.Term{..} <- takeRight (S.label nodeLabel)
  guard $ p term


-- | Check if the node is a non-terminal node with the given category.
hasCat :: Text -> Q.Expr Edge Q.Node
hasCat x = isNonTerm $ \S.NonTerm{..} -> x == cat


-- | Check if the node is a non-terminal node with the given attribute
-- and the corresponding value.
hasAttr :: Attr -> AttrVal -> Q.Expr Edge Q.Node
hasAttr x y = isNonTerm $ \S.NonTerm{..} -> isJust $ do
  y' <- M.lookup x morph
  guard $ y == y'


getAttr :: Attr -> Edge -> Maybe AttrVal
getAttr x S.Edge{..} = do
  S.NonTerm{..} <- takeLeft $ S.label nodeLabel
  M.lookup x morph


--------------------------------------------------------------------------------
-- SKL conversion
--------------------------------------------------------------------------------


liczbaSKL :: W.Number -> AttrVal
liczbaSKL x = case x of
  W.Singular -> "poj"
  W.Plural   -> "mno"


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------


-- | Base form attribute.
przypadek, przyimek, klasa, liczba :: Attr
przypadek = "przypadek"
przyimek = "przyim"
klasa = "klasa"
liczba = "liczba"


takeLeft :: Either a b -> Maybe a
takeLeft (Left x) = Just x
takeLeft (Right _) = Nothing


takeRight :: Either a b -> Maybe b
takeRight (Left _) = Nothing
takeRight (Right x) = Just x


maybeT :: Monad m => Maybe a -> MaybeT m a
maybeT = MaybeT . return
