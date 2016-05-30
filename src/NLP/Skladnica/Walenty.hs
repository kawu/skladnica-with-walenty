{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}


-- | A module responsible for identifying occurrences of the
-- Walenty valency dictionary entries in the Skladnica treebank.


module NLP.Skladnica.Walenty
(
) where

import           Data.Text         (Text)

import qualified NLP.Skladnica     as S
import qualified NLP.Walenty.Types as W


--------------------------------------------------------------------------------
-- Query Type
--
-- The idea is to translate, recursively, a given Walenty entry to a data type
-- representing the query that needs to be run, over a given node of the give
-- Skladnica tree, to verify whether the corresponding tree represents an
-- occurence of the valency entry.
--------------------------------------------------------------------------------


-- | Morphosyntactic attribute.
type Attr = Text
type Val = Text


-- | To mark queries on trees
data Tree
-- | To mark queries on nodes
data Node


-- | A boolean query to be evaluated over a node of a syntactic tree.
data Expr a where
  -- Simply a boolean value
  B :: Bool -> Expr a
  -- Logical conjunction
  And :: Expr a -> Expr a -> Expr a
  -- Logical disjunction
  Or :: Expr a -> Expr a -> Expr a
  -- An if then else expression
  IfThenElse
    :: Expr Node -- If
    -> Expr Tree -- Then
    -> Expr Tree -- Else
    -> Expr Tree
  -- Does the attribute have the given value?
  HasValue :: Attr -> Expr Val -> Expr Node
  -- Run the query over the current node
  Current :: Expr Node -> Expr Tree
  -- Plain value
  Plain :: Val -> Expr Val
  -- Case value (to resolve situations where the case can be structural or
  -- agreement)
  Case :: Val -> Expr Val
  -- The current node has to be an immediate parent of a tree which
  -- satisfied the given query/expression
  Child :: Expr Tree -> Expr Tree


-- | Build a complex `ancestor` tree expression.
-- Note that the resulting expression is infinite...
ancestor
  :: Expr Tree -- ^ A tree expression to be satisfied by the ancestor.
  -> Expr Tree
ancestor e = Child (Or e (ancestor e))


-- | AND query consisting of a list of expressions.
andQ :: [Expr a] -> Expr a
andQ (x : xs) = And x (andQ xs)
andQ [] = B True


-- | OR query consisting of a list of expressions.
orQ :: [Expr a] -> Expr a
orQ (x : xs) = Or x (orQ xs)
orQ [] = B False


-- | Convert the given verbal entry from Walenty to a query.
--
-- TODO: The following are ignored for the moment:
--
--   * Negativity
--   * Predicativity
--   * ...
--
querify :: W.Verb -> Expr Tree
querify verb = andQ
  [ Current . HasValue base . Plain $ W.base verb
  , frameQ (W.frame verb) ]


-- | A query expression for a frame.
--
-- TODO: At the moment it is not enfornced that each argument
-- is realized by a different tree node!
frameQ :: W.Frame -> Expr Tree
frameQ frame = andQ (map argumentQ frame)


-- | A query expression for an argument.
argumentQ :: W.Argument -> Expr Tree
argumentQ = undefined


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------


-- | Base form attribute.
base :: Attr
base = "base"
