{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


-- | A module responsible for identifying occurrences of the
-- Walenty valency dictionary entries in the Skladnica treebank.


module NLP.Skladnica.Walenty
( runTest
) where


import           Control.Monad             (forM_, guard)
-- import qualified Control.Monad.State.Strict as E
import qualified Control.Monad.Reader      as E
import           Control.Monad.Trans.Maybe (MaybeT (..))

import           Data.Either               (lefts)
import qualified Data.Map.Strict           as M
import           Data.Maybe                (isJust)
import           Data.Text                 (Text)
import qualified Data.Text.Lazy            as L
import           Data.Tree                 as R

import qualified NLP.Skladnica             as S
import qualified NLP.Walenty               as W
import qualified NLP.Walenty.Types         as W

import           Debug.Trace               (trace)


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
type AttrVal = Text


-- | Skladnica tree.
type SklTree = S.Tree S.Node S.IsHead


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
    :: Expr a -- If
    -> Expr a -- Then
    -> Expr a -- Else
    -> Expr a
  -- Does the current node satisfy the given predicate?
  Satisfy :: (S.Node -> Bool) -> Expr S.Node
  -- A predicate which has to be satisfied by the parent node
  -- and the current node at the same time (useful for agreement
  -- constraints)
  Satisfy2
    :: ( S.Node -- Parent node
      -> S.Node -- Current node
      -> Bool )
    -> Expr S.Node
  -- Run the query over the current node
  Current :: Expr S.Node -> Expr SklTree
  -- The current node has to be an immediate parent of a tree which
  -- satisfied the given query/expression
  Child :: Expr SklTree -> Expr SklTree
  -- Like `Child` but doesn't update the parent node
  -- (useful for agreement constraints)
  SkipChild :: Expr SklTree -> Expr SklTree
  -- Like `Child`, but applies to the head child only
  Head :: Expr SklTree -> Expr SklTree
  -- Check that the tree is non-branching
  NonBranching :: Expr SklTree


-- | Build a complex `ancestor` tree expression.
-- Note that the resulting expression is infinite...
ancestor
  :: Expr SklTree -- ^ A tree expression to be satisfied by the ancestor.
  -> Expr SklTree
ancestor e = Child (Or e (ancestor e))


-- | Check if one of the nodes present on the trunk
-- (the path to the anchor, anchor including)
-- satisfies the predicate.
trunk
  :: Expr S.Node
  -> Expr SklTree
trunk e = Or (Current e) (Head (trunk e))


-- | AND query consisting of a list of expressions.
andQ :: [Expr a] -> Expr a
andQ (x : xs) = And x (andQ xs)
andQ [] = B True


-- | OR query consisting of a list of expressions.
orQ :: [Expr a] -> Expr a
orQ (x : xs) = Or x (orQ xs)
orQ [] = B False


--------------------------------------------------------------------------------
-- Conversion to Query
--------------------------------------------------------------------------------


-- | Convert the given verbal entry from Walenty to a query.
--
-- TODO: The following are ignored for the moment:
--
--   * Negativity
--   * Predicativity
--   * ...
--
querify :: W.Verb -> Expr SklTree
querify verb = andQ
  [ trunk . hasBase . W.base $ verb
  , frameQ (W.frame verb) ]


-- | A query expression for a frame.
--
-- TODO: At the moment it is not enfornced that each argument
-- is realized by a different tree child node!
frameQ :: W.Frame -> Expr SklTree
-- frameQ frame = andQ $ map (Child . Child . argumentQ) frame
frameQ frame = andQ $ map (Child . metaArgQ) frame


-- | Handle (and ignore) nodes explicitely marked
-- with "fw", "fl", "ff".
metaArgQ :: W.Argument -> Expr SklTree
metaArgQ arg =
  IfThenElse
    isMetaNode
    (SkipChild argument)
    argument
  where
    argument = argumentQ arg
    isMetaNode = Current . isNonTerm $
      \S.NonTerm{..} -> cat `elem` ["fw", "fl", "ff"]


-- | A query expression for an argument.
-- TODO: function ignored.
argumentQ :: W.Argument -> Expr SklTree
argumentQ arg = orQ . map phraseQ $ W.phraseAlt arg


phraseQ :: W.Phrase -> Expr SklTree
phraseQ p = case p of
  W.Standard s -> stdPhraseQ s
  W.Special s -> specPhraseQ s


stdPhraseQ :: W.StdPhrase -> Expr SklTree
stdPhraseQ phrase = case phrase of
  W.NP{..} -> Current $ andQ
    [ hasCat "fno"
    , caseQ caseG
    , maybeQ agrNumber agreeNumQ
    ]
  W.PrepNP{..} -> andQ
    [ Current $ andQ
      [ hasCat "fpm"
      , hasAttr przyimek $ preposition
      , hasAttr klasa $ "rzecz"
      , caseQ caseG ]
    -- we "skip" the child so that potential agreement works
    -- between the parent of PP and the NP (not sure if this
    -- is necessary)
    , SkipChild $ andQ
        [ Current $ hasCat "fno"
        , Current $ maybeQ agrNumber agreeNumQ
        , case lexicalHead of
            [] -> B True
            xs -> trunk (hasBases xs)
        , case dependents of
            W.NAtr -> NonBranching
            _ -> B True
        ]
    ]
  _ -> B True


specPhraseQ :: W.SpecPhrase -> Expr SklTree
specPhraseQ _ = B True


maybeQ :: Maybe a -> (a -> Expr b) -> Expr b
maybeQ mx e = case mx of
  Nothing -> B True
  Just x -> e x


-- | Skladnica case value based on the Walenty case value.
caseQ :: W.Case -> Expr S.Node
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
        then B True
        else orQ $ map (hasAttr przypadek) xs


agreeNumQ :: W.Agree W.Number -> Expr S.Node
agreeNumQ agreeNum = case agreeNum of
  W.Agree -> Satisfy2 $ \parent child -> isJust $ do
    x <- getAttr liczba parent
    y <- getAttr liczba child
    guard $ x == y
  W.Value v -> hasAttr liczba $ liczbaSKL v
  -- W.Value v -> B True


-- | Check if the node is a terminal node with one of the given base values.
hasBases :: [Text] -> Expr S.Node
hasBases xs = isTerm $ \S.Term{..} -> base `elem` map L.fromStrict xs


-- | Check if the node is a terminal node with the given base.
hasBase :: Text -> Expr S.Node
-- hasBase x = isTerm $ \S.Term{..} -> L.fromStrict x == base
hasBase x = hasBases [x]


isNonTerm :: (S.NonTerm -> Bool) -> Expr S.Node
isNonTerm p = Satisfy $ \S.Node{..} -> isJust $ do
  nonTerm@S.NonTerm{..} <- takeLeft label
  guard $ p nonTerm


isTerm :: (S.Term -> Bool) -> Expr S.Node
isTerm p = Satisfy $ \S.Node{..} -> isJust $ do
  term@S.Term{..} <- takeRight label
  guard $ p term


-- | Check if the node is a non-terminal node with the given category.
hasCat :: Text -> Expr S.Node
hasCat x = isNonTerm $ \S.NonTerm{..} -> L.fromStrict x == cat


-- | Check if the node is a non-terminal node with the given attribute
-- and the corresponding value.
hasAttr :: Attr -> AttrVal -> Expr S.Node
hasAttr x y = isNonTerm $ \S.NonTerm{..} -> isJust $ do
  y' <- M.lookup (L.fromStrict x) morph
  guard $ L.fromStrict y == y'


getAttr :: Attr -> S.Node -> Maybe AttrVal
getAttr x S.Node{..} = do
  S.NonTerm{..} <- takeLeft label
  L.toStrict <$> M.lookup (L.fromStrict x) morph


--------------------------------------------------------------------------------
-- Query Evaluation
--------------------------------------------------------------------------------


-- | State underlying evaluation; we store information about the parent node
-- just in case.
data EvalState = EvalState
  { parentTree :: SklTree
  } deriving (Show, Eq, Ord)


-- | Evaluation monad.
-- type Eval = E.State EvalState
type Eval = E.Reader EvalState


-- | Run the evaluation monad.
runEval
  :: Expr SklTree
  -> SklTree -- ^ Parent tree
  -> SklTree -- ^ Child tree
  -> Bool
runEval expr parent child =
  -- E.evalState (evaluate expr child) (EvalState parent)
  E.runReader (evaluate expr child) (EvalState parent)


-- | Set the underlying parent tree to the new one.
withParent :: SklTree -> Eval a -> Eval a
withParent t = E.withReader $ \_ -> EvalState {parentTree = t}


-- | Get the current parent node.
getParent :: Eval SklTree
getParent = parentTree <$> E.ask


-- | Evaluate the query w.r.t. the given Skladnica tree.
--
-- TODO: Parent tree/node could be moved to an underlying state
--
evaluate
  :: Expr a -- ^ Expression to evaluate
  -> a      -- ^ Element on which to run the evaluation (tree, node, etc.)
  -> Eval Bool
evaluate (B x) _ = return x
evaluate (And x y) t =
  (&&) <$> evaluate x t <*> evaluate y t
evaluate (Or x y) t =
  (||) <$> evaluate x t <*> evaluate y t
evaluate (IfThenElse b e1 e2) t = do
  r <- evaluate b t
  if r
    then evaluate e1 t
    else evaluate e2 t
evaluate (Satisfy p) node = return (p node)
evaluate (Current e) t = evaluate e (S.rootLabel t)
evaluate (Child e) t = withParent t $
  or <$> sequence
    [ evaluate e s
    | (s, _) <- S.subForest t ]
evaluate (SkipChild e) t =
  or <$> sequence
    [ evaluate e s
    | (s, _) <- S.subForest t ]
evaluate (Head e) t = withParent t $
  or <$> sequence
    [ evaluate e s
    | (s, S.HeadYes) <- S.subForest t ]
evaluate (Satisfy2 p) childNode = do
  parentNode <- S.rootLabel <$> getParent
  return $ p parentNode childNode
evaluate NonBranching t = case S.subForest t of
  [] -> return True
  [x] -> evaluate NonBranching (fst x)
  _ -> return False


-- | Find tree node satisfying the given query expression.
findNodes
  :: Expr SklTree
  -> SklTree
  -> [SklTree]
-- findNodes e t = filter (evaluate Nothing e) (subTrees t)
findNodes e t =
  let xs = map fst $ S.subForest t
   in [x | x <- xs, runEval e t x] ++
      concatMap (findNodes e) xs


-- -- | Take all subtrees of the given skladnica tree.
-- subTrees :: SklTree -> [SklTree]
-- subTrees t = t : concatMap subTrees (map fst $ S.subForest t)


------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------


-- | Read all verb entries from Walenty and search for them
-- in Skladnica treebank.
runTest
  :: FilePath -- ^ Skladnica XML file
  -> FilePath -- ^ Walenty file
  -> FilePath -- ^ Walenty Expansion file
  -> IO ()
runTest skladnicaXML walentyPath expansionPath = do
  -- read verbal entries from Walenty
  walenty <- lefts <$> W.readWalenty expansionPath walentyPath
  -- take nodes from Skladnica XML file
  nodesDAG <- S.mkDAG <$> S.readTop skladnicaXML
  -- construct skladnica tree(s); normally there is only
  -- one chosen tree
  let sklTree = S.forest S.chosen 0 nodesDAG !! 0
      simpLab = L.unpack . either S.cat S.orth . S.label
  putStrLn . R.drawTree . fmap simpLab . S.simplify $ sklTree
  forM_ walenty $ \verb -> do
    print verb
    putStrLn ""
    let expr = querify verb
        mweTrees = findNodes expr sklTree
    putStrLn . R.drawForest . map (fmap simpLab . S.simplify) $ mweTrees


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
category, przypadek, przyimek, klasa, liczba :: Attr
-- base = "base"
category  = "category"
przypadek = "przypadek"
przyimek = "przyim"
klasa = "klasa"
liczba = "liczba"


takeLeft (Left x) = Just x
takeLeft (Right _) = Nothing


takeRight (Left _) = Nothing
takeRight (Right x) = Just x


maybeT :: Monad m => Maybe a -> MaybeT m a
maybeT = MaybeT . return
