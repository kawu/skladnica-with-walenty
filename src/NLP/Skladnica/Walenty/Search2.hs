{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


-- | A module responsible for identifying occurrences of the
-- Walenty valency dictionary entries in the Skladnica treebank.


module NLP.Skladnica.Walenty.Search2
( SklTree
, terminals
, Expr (..)
-- , querify
-- , findNodes
-- , markNodes
-- , markOne
-- , markAll

-- -- * Primitives
-- , andQ
-- , anyChild
-- , ancestor
-- , anyAncestor
-- , hasBase
-- , hasTag
-- , hasOrth
-- -- , hasOrths
) where


import           Control.Applicative        (empty, (<|>))
import           Control.Monad              (guard)
import qualified Control.Monad.State.Strict as E
-- import qualified Control.Monad.Reader      as E
-- import qualified Control.Monad.RWS.Strict  as E
import           Control.Monad.Trans.Maybe  (MaybeT (..), mapMaybeT)

-- import           Data.Foldable             (foldMap)
import           Data.List                  (foldl')
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (catMaybes, fromMaybe, isJust)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Tree                  as R
-- import qualified Data.Text.Lazy            as L

import qualified NLP.Skladnica              as S
import qualified NLP.Walenty.Types          as W

-- import           Debug.Trace               (trace)


-- | Morphosyntactic attribute.
type Attr = Text
type AttrVal = Text


-- | Skladnica tree.
type SklTree = S.Tree S.Node S.IsHead


-- | Retrieve terminal leaves of the given tree.
terminals
  :: SklTree
  -> [S.Term]
terminals =
  let getTerm S.Node{..} = case label of
        Left _  -> []
        Right t -> [t]
  in foldMap getTerm . S.simplify


--------------------------------------------------------------------------------
-- Query Type
--
-- The idea is to translate, recursively, a given Walenty entry to a data type
-- representing the query that needs to be run, over a given node of the give
-- Skladnica tree, to verify whether the corresponding tree represents an
-- occurence of the valency entry.
--------------------------------------------------------------------------------


-- | A datatype which allows to distinguish node expressions from tree
-- expressions.  Before we were marking this information as the last
-- parameter of the `Expr` type, but then it seemed impossible to
-- `transform` the underlying node type `n`.
data Type
  = Node   -- ^ Node expression
  | Tree   -- ^ Tree expression


-- | A boolean query to be evaluated over a node of a syntactic tree.
-- Type `n` stands for node type.
data Expr n a where
  -- Mark the given node as interesting (can be used, for instance,
  -- to mark lexically constrained nodes)
  Mark :: Expr n Node
  -- Simply a boolean value
  B :: Bool -> Expr n a
  -- Logical conjunction
  And :: Expr n a -> Expr n a -> Expr n a
  -- Logical disjunction
  Or :: Expr n a -> Expr n a -> Expr n a
  -- An if then else expression
  IfThenElse
    :: Expr n a -- If
    -> Expr n a -- Then
    -> Expr n a -- Else
    -> Expr n a
  -- Does the current node satisfy the given predicate?
  SatisfyNode :: (n -> Bool) -> Expr n Node
  -- Like `SatisfyNode`, but works on trees; be aware that if you use `Satisfy`
  -- over a tree to check some lexical constraints, the corresponding lexical
  -- nodes will not be marked.
  SatisfyTree :: (R.Tree n -> Bool) -> Expr n Tree
  -- Run the expression over the current node
  Current :: Expr n Node -> Expr n Tree
  -- The current node has to be an immediate parent of a tree which
  -- satisfied the given query/expression
  Child :: (n -> Bool) -> Expr n Tree -> Expr n Tree
  -- Check that the tree is non-branching
  NonBranching :: Expr n Tree


-- | Take any child.
anyChild :: Expr n Tree -> Expr n Tree
anyChild = Child $ const True


-- | Check if one of the nodes present on a path of nodes satisfiyng the given
-- predicate satisfies the given expression.
ancestor
  :: (n -> Bool)
  -> Expr n Node
  -> Expr n Tree
ancestor p e = Or (Current e) (Child p (ancestor p e))


-- | Take any ancestor.
anyAncestor
  :: Expr n Node
  -> Expr n Tree
anyAncestor e = Or (Current e) (anyChild (anyAncestor e))


-- | AND query consisting of a list of expressions.
andQ :: [Expr n a] -> Expr n a
andQ (x : xs) = And x (andQ xs)
andQ [] = B True


-- | OR query consisting of a list of expressions.
orQ :: [Expr n a] -> Expr n a
orQ (x : xs) = Or x (orQ xs)
orQ [] = B False


-- | Consider expression if the argument is `Just`.
maybeQ :: Maybe a -> (a -> Expr n b) -> Expr n b
maybeQ mx e = case mx of
  Nothing -> B True
  Just x -> e x


-- | Transform the expression so that it works on a different type of nodes
-- (from which we can easily retrieve nodes of the original type).
transform
  :: (m -> n)
  -> Expr n a
  -> Expr m a
transform f expr = case expr of
  Mark -> Mark
  B b -> B b
  And e1 e2 -> And (transform f e1) (transform f e2)
  Or e1 e2 -> Or (transform f e1) (transform f e2)
  IfThenElse e1 e2 e3 -> IfThenElse
    (transform f e1)
    (transform f e2)
    (transform f e3)
  SatisfyNode p -> SatisfyNode (p . f)
  SatisfyTree p -> SatisfyTree (p . fmap f)
  Current e -> Current (transform f e)
  Child p e -> Child (p . f) (transform f e)
  NonBranching -> NonBranching


--------------------------------------------------------------------------------
-- Query Evaluation
--------------------------------------------------------------------------------


-- | Evaluation monad.  The `S.Set i` component contains the selected
-- (marked) nodes' identifiers.
type Mark i n = E.State (Set.Set i)
-- type Mark i n = E.RWS (MarkState n) () (Set.Set i)


-- -- | Run the `Mark` monad, i.e. match the given expression
-- -- against the given (child) tree and, if the match is found,
-- -- returned the tree with the expression matched.
-- runMark
--   :: (Ord i)
--   => (n -> i)   -- ^ Get ID of a node
--   -> Expr n Tree
--   -> R.Tree n   -- ^ Parent tree
--   -> R.Tree n   -- ^ Child tree
--   -> Set.Set i
-- runMark getID expr parent child =
--   trim $ E.runRWS rws (EvalState parent) Set.empty
--   where
--     rws = evaluate getID expr child
--     trim (True, s, _w) = s
--     trim (False, _, _) = Set.empty


-- | Evaluate the query over the given Skladnica tree.
markTree
  :: forall n i. Ord i
  => (n -> i)    -- ^ Function to get ID of a node
  -> Expr n Tree -- ^ Expression to evaluate
  -> R.Tree n    -- ^ Tree on which to run the evaluation
  -> Mark i n Bool
markTree getID expr =
  mark expr
  where
    mark :: Expr n Tree -> R.Tree n -> Mark i n Bool
    mark (B x) _ = return x
    mark (And x y) t =
      (&&) <$> mark x t <*> mark y t
    mark (Or x y) t =
      (||) <$> mark x t <*> mark y t
    mark (IfThenElse b e1 e2) t = do
      r <- mark b t
      if r
        then mark e1 t
        else mark e2 t
    mark (SatisfyTree p) t = return (p t)
    mark (Current e) t = markNode getID e (R.rootLabel t)
    mark (Child p e) t =
      or <$> sequence
        [ mark e child
        | child <- R.subForest t
        , p (R.rootLabel child) ]
    mark NonBranching t = case R.subForest t of
      [] -> return True
      [x] -> mark NonBranching x
      _ -> return False


-- | Evaluate the query over the given Skladnica node.
markNode :: Ord i => (n -> i) -> Expr n Node -> n -> Mark i n Bool
markNode getID expr =
  mark expr
  where
    mark Mark x = True <$ markID (getID x)
    mark (B x) _ = return x
    mark (And x y) t =
      (&&) <$> mark x t <*> mark y t
    mark (Or x y) t =
      (||) <$> mark x t <*> mark y t
    mark (IfThenElse b e1 e2) t = do
      r <- mark b t
      if r
        then mark e1 t
        else mark e2 t
    mark (SatisfyNode p) t = return (p t)


-- | Save the node in the underlying state.
markID :: Ord i => i -> Mark i n ()
markID = E.modify' . Set.insert


-- -- | Evaluate the query over the given Skladnica node.
-- markNode
--   :: forall n i. Ord i
--   => (n -> i)    -- ^ Function to get ID of a node
--   -> Expr n Node -- ^ Expression to evaluate
--   -> n           -- ^ Node on which to run the evaluation
--   -> Mark i n Bool
-- markNode getID expr = undefined
-- --   mark expr
-- --   where
-- --     mark :: Expr n Node -> n -> Mark i n Bool
-- --     mark Mark x = True <$ markID (getID x)
-- --     mark (B x) _ = return x
-- --     mark (And x y) t =
-- --       (&&) <$> mark x t <*> mark y t
-- --     mark (Or x y) t =
-- --       (||) <$> mark x t <*> mark y t
-- --     mark (IfThenElse b e1 e2) t = do
-- --       r <- mark b t
-- --       if r
-- --         then mark e1 t
-- --         else mark e2 t


-- -- | Evaluate the query w.r.t. the given Skladnica tree.
-- evaluate
--   :: (Ord i)
--   => (n -> i)   -- ^ Function to get ID of a node
--   -> Expr a     -- ^ Expression to evaluate
--   -> a          -- ^ Element on which to run the evaluation (tree, node, etc.)
--   -> Mark i n Bool
-- evaluate getID expr =
--   mark expr
--   where
-- --     mark Mark n = True <$ markNode (getID n)
--     mark (B x) _ = return x
--     mark (And x y) t =
--       (&&) <$> mark x t <*> mark y t
--     mark (Or x y) t =
--       (||) <$> mark x t <*> mark y t
--     mark (IfThenElse b e1 e2) t = do
--       r <- mark b t
--       if r
--         then mark e1 t
--         else mark e2 t
--     mark (Satisfy p) x = return (p x)
--     -- using `evaluate` below to avoid type-level voodoo...
--     mark (Current e) t = evaluate getID e (R.rootLabel t)
--     mark (Child p e) t = withParent t $ mark (SkipChild p e) t
--     mark (SkipChild p e) t =
--       or <$> sequence
--         [ mark e child
--         | child <- R.subForest t
--         , p (R.rootLabel child) ]
--     mark (Satisfy2 p) childNode = do
--       parentNode <- R.rootLabel <$> getParent
--       return $ p parentNode childNode
--     mark NonBranching t = case R.subForest t of
--       [] -> return True
--       [x] -> mark NonBranching x
--       _ -> return False
--
--
-- -- -- | Find tree node satisfying the given query expression.
-- -- markNodes
-- --   :: Expr SklTree
-- --   -> SklTree
-- --   -> [SklTree]
-- -- -- findNodes e t = filter (evaluate Nothing e) (subTrees t)
-- -- markNodes e t =
-- --   let xs = map fst $ S.subForest t
-- --    in catMaybes [runMark e t x | x <- xs] ++
-- --       concatMap (markNodes e) xs
--
--
-- -- -- | Match all the given expressions against all the nodes
-- -- -- in the given tree and mark the identified occurences.
-- -- markAll
-- --   :: Ord i
-- --   => (n -> i)
-- --   -> [Expr n (R.Tree n)]
-- --   -> R.Tree n
-- --   -> R.Tree (n, Set.Set i)
-- -- markAll es t =
-- --   foldl' (flip markOne) t es
--
--
-- -- | Mark the tree with an occurence of the given expression.
-- -- Anywhere in the tree.
-- markOne
--   :: (Ord i)
--   => (n -> i)
--      -- ^ Get ID of a node
--   -> Expr n (R.Tree n)
--      -- ^ The expression to evaluate over a given tree
--   -> R.Tree n
--      -- ^ The tree to evaluate the expression on
--   -> R.Tree (n, Set.Set i)
--      -- ^ The resulting tree, where nodes are possibly annotated
--      -- with the corresponding marked nodes
-- markOne getID e t =
--   -- TODO: the parent of the entire tree is set to the tree itself...
--   go t t
--   where
--     go parent tree = R.Node
--       { R.rootLabel =
--           ( R.rootLabel tree
--           , runMark getID e parent tree )
--       , R.subForest =
--           [ go tree child
--           | child <- R.subForest tree ] }


-- --------------------------------------------------------------------------------
-- -- Conversion to Query
-- --------------------------------------------------------------------------------
--
--
-- -- | Convert the given verbal entry from Walenty to a query.
-- --
-- -- TODO: The following are ignored for the moment:
-- --
-- --   * Negativity
-- --   * Predicativity
-- --   * ...
-- --
-- querify :: W.Verb -> Expr SklTree
-- querify verb = andQ
--   [ trunk . hasBase . W.base $ verb
--   , frameQ (W.frame verb) ]
--
--
-- -- | A query expression for a frame.
-- --
-- -- TODO: At the moment it is not enfornced that each argument
-- -- is realized by a different tree child node!
-- frameQ :: W.Frame -> Expr SklTree
-- -- frameQ frame = andQ $ map (Child . Child . argumentQ) frame
-- frameQ frame = andQ $ map (anyChild . metaArgQ) frame
--
--
-- -- | Pseudo-frame in which one or more of the arguments must be realized.
-- -- Useful within the context of constraints on dependents, which are
-- -- called "frames" but are not really.
-- --
-- -- TODO: Difficult choice, sometimes it seems that all dependents specified
-- -- in `RAtr` should be present, sometimes that only some of them...
-- pseudoFrameQ :: W.Frame -> Expr SklTree
-- -- pseudoFrameQ frame = orQ $ map (Child . metaArgQ) frame
-- pseudoFrameQ = frameQ
--
--
-- -- | Handle (and ignore) nodes explicitely marked
-- -- with "fw", "fl", "ff".
-- metaArgQ :: W.Argument -> Expr SklTree
-- metaArgQ arg =
--   IfThenElse
--     isMetaNode
--     (skipAnyChild argument)
--     argument
--   where
--     argument = argumentQ arg
--     isMetaNode = Current . isNonTerm $
--       \S.NonTerm{..} -> cat `elem` ["fw", "fl", "ff"]
--
--
-- -- | A query expression for an argument.
-- -- TODO: function ignored.
-- argumentQ :: W.Argument -> Expr SklTree
-- argumentQ arg = orQ . map phraseQ $ W.phraseAlt arg
--
--
-- phraseQ :: W.Phrase -> Expr SklTree
-- phraseQ p = case p of
--   W.Standard s -> stdPhraseQ s
--   W.Special s -> specPhraseQ s
--
--
-- stdPhraseQ :: W.StdPhrase -> Expr SklTree
-- stdPhraseQ phrase = case phrase of
--   W.NP{..} -> andQ
--     [ Current $ andQ
--       [ hasCat "fno"
--       , caseQ caseG
--       , maybeQ agrNumber agreeNumQ ]
--     , andQ
--       [ lexicalQ lexicalHead
--       , dependentsQ dependents ]
--     ]
--   W.PrepNP{..} -> andQ
--     [ Current $ andQ
--       [ hasCat "fpm"
--       , hasAttr przyimek $ preposition
--       , hasAttr klasa $ "rzecz"
--       , caseQ caseG ]
--     -- we "skip" the child so that potential agreement works
--     -- between the parent of PP and the NP (not sure if this
--     -- is necessary)
--     , skipAnyChild $ andQ
--         [ Current $ hasCat "fno"
--         , Current $ maybeQ agrNumber agreeNumQ
--         , lexicalQ lexicalHead
--         , dependentsQ dependents
--         ]
--     ]
--   -- Don't know how to handle this yet, and it is not handled
--   -- by the default handler below (which referes to dependents)
--   W.ComparP{} -> B False
--   -- By default we check if (a) lexical requirements are satisfied for the
--   -- argument itself, directly, or (b) for one of its children, which makes
--   -- sense for certain phrase types (e.g., `CP`)
--   p ->
--     let checkLex = andQ
--           [ lexicalQ (W.lexicalHead p)
--           , dependentsQ (W.dependents p) ]
--     in  checkLex `Or` skipAnyChild checkLex
--
-- --   W.CP{..} -> andQ
-- --     [ Current $ andQ
-- --       [ hasCat "fzd" ]
-- --     , anyChild $ andQ
-- --       [ lexicalQ lexicalHead
-- --       , dependentsQ dependents ]
-- --     ]
--
--
-- specPhraseQ :: W.SpecPhrase -> Expr SklTree
-- specPhraseQ p = case p of
--   W.XP{..} -> maybeQ xpVal phraseQ
--   -- TODO: not handled yet
--   W.Fixed{} -> B False
--   _ -> B True
-- -- specPhraseQ _ = B True
--
--
-- -- | Constraints on lexical heads.
-- lexicalQ :: [Text] -> Expr SklTree
-- lexicalQ xs = if null xs
--   then B True
--   else trunk (hasBases xs)
--
--
-- -- | Constraints stemming from the requirements over
-- -- the dependents.
-- dependentsQ :: W.Attribute -> Expr SklTree
-- dependentsQ deps = case deps of
--   -- no modifiers allowed
--   W.NAtr -> NonBranching
--   -- modifiers allowed but optional; TODO: we could check that all modifiers
--   -- present are consistent with the given `Atr` list.
--   W.Atr _ -> B True
--   -- TODO: not distinguished from `Atr` case.
--   W.Atr1 _ -> B True
--   -- at least one of the attributes given in the list must be present
--   W.RAtr xs -> pseudoFrameQ xs
--   -- TODO: we should check that there is at most one modifier.
--   W.RAtr1 xs -> pseudoFrameQ xs
--   _ -> B True
--
--
-- -- | Skladnica case value based on the Walenty case value.
-- caseQ :: W.Case -> Expr S.Node
-- caseQ c =
--   pr $ case c of
--     W.Nominative -> sg "mian"
--     W.Genitive -> sg "dop"
--     W.Dative -> sg "cel"
--     W.Accusative -> sg "bier"
--     W.Instrumental -> sg "narz"
--     W.Locative -> sg "miej"
--     W.Partitive -> ["dop", "bier"]
--     -- TODO: structural case should depend on the function, can be
--     -- precomputed at the compilation stage.
--     W.Structural -> ["mian", "dop", "bier"]
--     -- TODO: not sure if can be resolved at the compilation stage
--     W.Agreement -> []
--     W.PostPrep -> sg "pop"
--     -- TODO: not handled by Agata's specification
--     W.Predicative -> []
--     where
--       sg x = [x]
--       pr xs = if null xs
--         then B True
--         else orQ $ map (hasAttr przypadek) xs
--
--
-- agreeNumQ :: W.Agree W.Number -> Expr S.Node
-- agreeNumQ agreeNum = case agreeNum of
--   W.Agree -> Satisfy2 $ \parent child -> isJust $ do
--     x <- getAttr liczba parent
--     y <- getAttr liczba child
--     guard $ x == y
--   W.Value v -> hasAttr liczba $ liczbaSKL v
--   -- W.Value v -> B True
--
--
-- -- | Check if the node is a terminal node with one of the given base values.
-- hasBases :: [Text] -> Expr S.Node
-- hasBases xs = isTerm $ \S.Term{..} -> base `elem` xs
--
--
-- -- | Check if the node is a terminal node with the given base.
-- hasBase :: Text -> Expr S.Node
-- hasBase x = hasBases [x]
--
--
-- -- | Check if the node is a terminal node with one of the given orth values.
-- hasOrth :: (Text -> Bool) -> Expr S.Node
-- hasOrth p = isTerm $ \S.Term{..} -> p orth
--
--
-- -- -- | Check if the node is a terminal node with one of the given orth values.
-- -- hasOrths :: [Text] -> Expr S.Node
-- -- hasOrths xs = hasOrth (`elem` xs)
--
--
-- -- -- | Check if the node is a terminal node with the given orth value.
-- -- hasOrth :: Text -> Expr S.Node
-- -- hasOrth x = hasOrths [x]
--
--
-- -- | Check if the node is a terminal node with the given tag value.
-- hasTag :: Text -> Expr S.Node
-- hasTag x = isTerm $ \S.Term{..} -> tag == x
--
--
-- isNonTerm :: (S.NonTerm -> Bool) -> Expr S.Node
-- isNonTerm p = Satisfy $ \S.Node{..} -> isJust $ do
--   nonTerm@S.NonTerm{..} <- takeLeft label
--   guard $ p nonTerm
--
--
-- isTerm :: (S.Term -> Bool) -> Expr S.Node
-- isTerm p = Satisfy $ \S.Node{..} -> isJust $ do
--   term@S.Term{..} <- takeRight label
--   guard $ p term
--
--
-- -- | Check if the node is a non-terminal node with the given category.
-- hasCat :: Text -> Expr S.Node
-- hasCat x = isNonTerm $ \S.NonTerm{..} -> x == cat
--
--
-- -- | Check if the node is a non-terminal node with the given attribute
-- -- and the corresponding value.
-- hasAttr :: Attr -> AttrVal -> Expr S.Node
-- hasAttr x y = isNonTerm $ \S.NonTerm{..} -> isJust $ do
--   y' <- M.lookup x morph
--   guard $ y == y'
--
--
-- getAttr :: Attr -> S.Node -> Maybe AttrVal
-- getAttr x S.Node{..} = do
--   S.NonTerm{..} <- takeLeft label
--   M.lookup x morph
--
--
-- --------------------------------------------------------------------------------
-- -- SKL conversion
-- --------------------------------------------------------------------------------
--
--
-- liczbaSKL :: W.Number -> AttrVal
-- liczbaSKL x = case x of
--   W.Singular -> "poj"
--   W.Plural   -> "mno"


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
