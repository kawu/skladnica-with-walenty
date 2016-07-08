{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


-- | A module responsible for identifying occurrences of the
-- Walenty valency dictionary entries in the Skladnica treebank.


module NLP.Skladnica.Walenty.Search2
(
-- * Query language
-- ** Primitives
  Expr (..)
, Type (..)
, markOne
, markAll
-- ** Constructions
, andQ
, orQ
, anyChild
, ancestor
, anyAncestor
, ifThenElse
, maybeQ
) where


import           Control.Applicative        (empty, (<|>))
import qualified Control.Monad.State.Strict as E

import           Data.List                  (foldl')
import qualified Data.Set                   as S
import qualified Data.Tree                  as R


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
  Mark :: Expr n 'Node
  -- Simply a boolean value
  B :: Bool -> Expr n a
  -- Logical conjunction
  And :: Expr n a -> Expr n a -> Expr n a
  -- Logical disjunction
  Or :: Expr n a -> Expr n a -> Expr n a
  -- Does the current node satisfy the given predicate?
  SatisfyNode :: (n -> Bool) -> Expr n 'Node
  -- Like `SatisfyNode`, but works on trees; be aware that if you use `Satisfy`
  -- over a tree to check some lexical constraints, the corresponding lexical
  -- nodes will not be marked.
  SatisfyTree :: (R.Tree n -> Bool) -> Expr n 'Tree
  -- Run the expression over the current node
  Current :: Expr n 'Node -> Expr n 'Tree
  -- The current node has to be an immediate parent of a tree which
  -- satisfied the given query/expression
  Child :: (n -> Bool) -> Expr n 'Tree -> Expr n 'Tree
  -- Check that the tree is non-branching
  NonBranching :: Expr n 'Tree


-- | If-then-else expression.
ifThenElse :: Expr n a -> Expr n a -> Expr n a -> Expr n a
ifThenElse e1 e2 e3 = (e1 `And` e2) `Or` e3


-- | Take any child.
anyChild :: Expr n 'Tree -> Expr n 'Tree
anyChild = Child $ const True


-- | Check if one of the nodes present on a path of nodes satisfiyng the given
-- predicate satisfies the given expression.
ancestor
  :: (n -> Bool)
  -> Expr n 'Node
  -> Expr n 'Tree
ancestor p e = Or (Current e) (Child p (ancestor p e))


-- | Take any ancestor.
anyAncestor
  :: Expr n 'Node
  -> Expr n 'Tree
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


--------------------------------------------------------------------------------
-- Node Transformation
--------------------------------------------------------------------------------

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
type Mark i = E.State (S.Set i)
-- type Mark i n = E.RWS (MarkState n) () (S.Set i)


-- | Run the `Mark` monad, i.e. match the given expression
-- against the given (child) tree and, if the match is found,
-- returned the tree with the expression matched.
runMark
  :: (Ord i)
  => (n -> i)   -- ^ Get ID of a node
  -> Expr n 'Tree
  -> R.Tree n   -- ^ Child tree
  -> S.Set i
runMark getID expr child =
  trim $ E.runState comp S.empty
  where
    comp = markTree getID expr child
    trim (True, s) = s
    trim (False, _) = S.empty


-- | Evaluate the query over the given Skladnica tree.
markTree
  :: forall n i. Ord i
  => (n -> i)     -- ^ Function to get ID of a node
  -> Expr n 'Tree -- ^ Expression to evaluate
  -> R.Tree n     -- ^ Tree on which to run the evaluation
  -> Mark i Bool
markTree getID expr =
  mark expr
  where
    mark :: Expr n 'Tree -> R.Tree n -> Mark i Bool
    mark (B x) _ = return x
    mark (And x y) t = markAnd mark x y t
    mark (Or x y) t = markOr mark x y t
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
markNode :: forall i n. Ord i => (n -> i) -> Expr n 'Node -> n -> Mark i Bool
markNode getID expr =
  mark expr
  where
    mark :: Expr n 'Node -> n -> Mark i Bool
    mark Mark x = True <$ markID (getID x)
    mark (B x) _ = return x
    mark (And x y) t = markAnd mark x y t
    mark (Or x y) t = markOr mark x y t
    mark (SatisfyNode p) t = return (p t)


-- | Save the node in the underlying state.
markID :: Ord i => i -> Mark i ()
markID = E.modify' . S.insert


-- | Generalized `And` handler.
markAnd :: (a -> b -> Mark i Bool) -> a -> a -> b -> Mark i Bool
markAnd mark x y t = do
  b <- mark x t
  if b
    then mark y t
    else return False


-- | Generalized `Or` handler.
markOr :: (a -> b -> Mark i Bool) -> a -> a -> b -> Mark i Bool
markOr mark x y t = do
  s <- E.get
  b <- mark x t
  if b
    then return True
    -- note that even when the first expression fails, it could have modified
    -- the underlying state, thus we need to restore it
    else E.put s >> mark y t


-- | Match all the given expressions against all the nodes
-- in the given tree and mark the identified occurences.
markAll
  :: Ord i
  => (n -> i)
  -> [Expr n 'Tree]
  -> R.Tree n
  -> R.Tree (n, S.Set i)
markAll getID es t =
  let update tree expr = markOne getID expr tree
  in  foldl' update (fmap (, S.empty) t) es


-- | Mark the tree with an occurence of the given expression.
-- Anywhere in the tree.  If other occurences were already found,
-- the new ones will be joined with the old ones through Set union.
markOne
  :: (Ord i)
  => (n -> i)
     -- ^ Get ID of a node
  -> Expr n 'Tree
     -- ^ The expression to evaluate over a given tree
  -> R.Tree (n, S.Set i)
     -- ^ The tree to evaluate the expression on
  -> R.Tree (n, S.Set i)
     -- ^ The resulting tree, where nodes are possibly annotated
     -- with the corresponding marked nodes
markOne getID0 e0 t =
  go t
  where
    e = transform fst e0
    getID = getID0 . fst
    go tree = R.Node
      { R.rootLabel =
          let idSet = runMark getID e tree
          in  onSnd (S.union idSet) (R.rootLabel tree)
      , R.subForest =
          [ go child
          | child <- R.subForest tree ] }
    onSnd f (x, y) = (x, f y)
