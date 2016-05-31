{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


-- | A module responsible for identifying occurrences of the
-- Walenty valency dictionary entries in the Skladnica treebank.


module NLP.Skladnica.Walenty
( runTest
) where


import           Control.Monad               (forM_, guard)
import           Control.Applicative         ((<|>), empty)
-- import qualified Control.Monad.State.Strict as E
import qualified Control.Monad.Reader        as E
import           Control.Monad.Trans.Maybe   (MaybeT (..), mapMaybeT)

import           Data.Either                 (lefts)
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (isJust, mapMaybe, catMaybes)
import qualified Data.Set                    as S
import           Data.Text                   (Text)
import qualified Data.Text.Lazy              as L
import           Data.Tree                   as R

import qualified System.Directory            as D
import qualified System.FilePath.Find        as F

import qualified NLP.Skladnica               as S
import qualified NLP.Walenty                 as W
import qualified NLP.Walenty.Types           as W

import qualified NLP.Skladnica.Walenty.Prune as P

-- import           Debug.Trace               (trace)


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
  -- satisfied the given query/expression; headedness of the parent-child
  -- relation can be checked.
  Child :: (S.IsHead -> Bool) -> Expr SklTree -> Expr SklTree
  -- Like `Child` but doesn't update the parent node
  -- (useful for agreement constraints)
  SkipChild :: (S.IsHead -> Bool) -> Expr SklTree -> Expr SklTree
  -- Check that the tree is non-branching
  NonBranching :: Expr SklTree


-- | Build a complex `ancestor` tree expression.
-- Note that the resulting expression is infinite...
ancestor
  :: Expr SklTree -- ^ A tree expression to be satisfied by the ancestor.
  -> Expr SklTree
ancestor e = anyChild (Or e (ancestor e))


-- | Check if one of the nodes present on the trunk
-- (the path to the anchor, anchor including)
-- satisfies the predicate.
trunk
  :: Expr S.Node
  -> Expr SklTree
trunk e = Or (Current e) (headChild (trunk e))


-- | AND query consisting of a list of expressions.
andQ :: [Expr a] -> Expr a
andQ (x : xs) = And x (andQ xs)
andQ [] = B True


-- | OR query consisting of a list of expressions.
orQ :: [Expr a] -> Expr a
orQ (x : xs) = Or x (orQ xs)
orQ [] = B False


maybeQ :: Maybe a -> (a -> Expr b) -> Expr b
maybeQ mx e = case mx of
  Nothing -> B True
  Just x -> e x


-- | Take a head child.
headChild :: Expr SklTree -> Expr SklTree
headChild = Child $ \h -> h == S.HeadYes


-- | Take any child.
anyChild :: Expr SklTree -> Expr SklTree
anyChild = Child $ const True


-- | Skip any child.
skipAnyChild :: Expr SklTree -> Expr SklTree
skipAnyChild = SkipChild $ const True


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
frameQ frame = andQ $ map (anyChild . metaArgQ) frame


-- | Pseudo-frame in which one or more of the arguments must be realized.
-- Useful within the context of constraints on dependents, which are
-- called "frames" but are not really.
--
-- TODO: Difficult choice, sometimes it seems that all dependents specified
-- in `RAtr` should be present, sometimes that only some of them...
pseudoFrameQ :: W.Frame -> Expr SklTree
-- pseudoFrameQ frame = orQ $ map (Child . metaArgQ) frame
pseudoFrameQ = frameQ


-- | Handle (and ignore) nodes explicitely marked
-- with "fw", "fl", "ff".
metaArgQ :: W.Argument -> Expr SklTree
metaArgQ arg =
  IfThenElse
    isMetaNode
    (skipAnyChild argument)
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
  W.NP{..} -> andQ
    [ Current $ andQ
      [ hasCat "fno"
      , caseQ caseG
      , maybeQ agrNumber agreeNumQ ]
    , andQ
      [ lexicalQ lexicalHead
      , dependentsQ dependents ]
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
    , skipAnyChild $ andQ
        [ Current $ hasCat "fno"
        , Current $ maybeQ agrNumber agreeNumQ
        , lexicalQ lexicalHead
        , dependentsQ dependents
        ]
    ]
  -- Don't know how to handle this yet, and it is not handled
  -- by the default handler below (which referes to dependents)
  W.ComparP{} -> B False
  -- By default we check if (a) lexical requirements are satisfied for the
  -- argument itself, directly, or (b) for one of its children, which makes
  -- sense for certain phrase types (e.g., `CP`)
  p ->
    let checkLex = andQ
          [ lexicalQ (W.lexicalHead p)
          , dependentsQ (W.dependents p) ]
    in  checkLex `Or` skipAnyChild checkLex

--   W.CP{..} -> andQ
--     [ Current $ andQ
--       [ hasCat "fzd" ]
--     , anyChild $ andQ
--       [ lexicalQ lexicalHead
--       , dependentsQ dependents ]
--     ]


specPhraseQ :: W.SpecPhrase -> Expr SklTree
specPhraseQ p = case p of
  W.XP{..} -> maybeQ xpVal phraseQ
  -- TODO: not handled yet
  W.Fixed{} -> B False
  _ -> B True
-- specPhraseQ _ = B True


-- | Constraints on lexical heads.
lexicalQ :: [Text] -> Expr SklTree
lexicalQ xs = if null xs
  then B True
  else trunk (hasBases xs)


-- | Constraints stemming from the requirements over
-- the dependents.
dependentsQ :: W.Attribute -> Expr SklTree
dependentsQ deps = case deps of
  -- no modifiers allowed
  W.NAtr -> NonBranching
  -- modifiers allowed but optional; TODO: we could check that all modifiers
  -- present are consistent with the given `Atr` list.
  W.Atr _ -> B True
  -- TODO: not distinguished from `Atr` case.
  W.Atr1 _ -> B True
  -- at least one of the attributes given in the list must be present
  W.RAtr xs -> pseudoFrameQ xs
  -- TODO: we should check that there is at most one modifier.
  W.RAtr1 xs -> pseudoFrameQ xs
  _ -> B True


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
evaluate (Child p e) t = withParent t $ evaluate (SkipChild p e) t
--   or <$> sequence
--     [ evaluate e s
--     | (s, h) <- S.subForest t, p h ]
evaluate (SkipChild p e) t =
  or <$> sequence
    [ evaluate e s
    | (s, h) <- S.subForest t, p h ]
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


--------------------------------------------------------------------------------
-- Alternative Query Evaluation
--------------------------------------------------------------------------------


-- | State underlying evaluation; we store information about the parent node
-- just in case.
type MarkState = EvalState


-- | Evaluation monad.
-- type Eval = E.State EvalState
type Mark = MaybeT (E.Reader MarkState)


-- | Run the evaluation monad.
runMark
  :: Expr SklTree
  -> SklTree -- ^ Parent tree
  -> SklTree -- ^ Child tree
  -> Maybe SklTree
runMark expr parent child =
  flip E.runReader (EvalState parent) $
    runMaybeT $ mark expr child


-- | Set the underlying parent tree to the new one.
withParent' :: SklTree -> Mark a -> Mark a
withParent' t = mapMaybeT $ E.withReader (\_ -> EvalState {parentTree = t})


-- | Get the current parent node.
getParent' :: Mark SklTree
getParent' = E.asks parentTree


-- | Evaluate the query w.r.t. the given Skladnica tree.
--
mark
  :: Expr a -- ^ Expression to evaluate
  -> a      -- ^ Element on which to run the evaluation (tree, node, etc.)
  -> Mark a -- ^ Potentially marked element, if satisfies the given expression
mark (B b) x = maybeT $ case b of
  False -> Nothing
  True  -> Just x
mark (And x y) t =
  mark x t >>= mark y
mark (Or x y) t = mark x t <|> mark y t
mark (IfThenElse b e1 e2) t = do
  (mark b t >>= mark e1) <|> mark e2 t
mark (Satisfy p) node = maybeT $ case p node of
  False -> Nothing
  True  -> Just node
mark (Current e) t =
  -- note that here we assume that nodes do not change
  t <$ mark e (S.rootLabel t)
mark (Child p e) t = withParent' t $ mark (SkipChild p e) t
mark (SkipChild p e) t = do
  -- here we need to enforce that one of the children
  -- satisfies the expression `e`.
  forest <- go $ S.subForest t
  return $ t {S.subForest = forest}
  where
    go [] = empty
    go (x@(s, h) : xs)
      -- below we mark the processed element
      | p h = (\y -> (y, S.HeadYes) : xs) <$> mark e s
          <|> (x :) <$> go xs
      | otherwise = (x :) <$> go xs
mark (Satisfy2 p) childNode = do
  parentNode <- S.rootLabel <$> getParent'
  if p parentNode childNode
    then return childNode
    else empty
mark NonBranching t = case S.subForest t of
  [] -> return t
  [x] -> t <$ mark NonBranching (fst x)
  _ -> empty


-- | Find tree node satisfying the given query expression.
findNodes'
  :: Expr SklTree
  -> SklTree
  -> [SklTree]
-- findNodes e t = filter (evaluate Nothing e) (subTrees t)
findNodes' e t =
  let xs = map fst $ S.subForest t
   in catMaybes [runMark e t x | x <- xs] ++
      concatMap (findNodes' e) xs


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
  expMap <- W.readExpMap expansionPath
  -- read *lexicalized* verbal entries from Walenty
  walenty <-
       S.toList . S.fromList
     . map (W.expandVerb expMap)
     . mapMaybe P.pruneVerb
     . lefts
    <$> W.readWalenty walentyPath
  putStr "Number of lexical entries: " >> print (length walenty)
  -- find all XML files
  xmlFiles <- getXmlFiles skladnicaDir
  -- per each XML file...
  forM_ xmlFiles $ \skladnicaXML -> do
    putStrLn skladnicaXML
    sklForest <- forestFromXml skladnicaXML
    forM_ sklForest $ \sklTree -> do
      -- putStrLn $ showTree sklTree
      forM_ walenty $ \verb -> do
        -- print verb
        -- putStrLn ""
        let expr = querify verb
            mweTrees = findNodes' expr sklTree
        E.when (not $ null mweTrees) $ do
          putStrLn "" >> print verb >> putStrLn ""
          -- putStrLn . R.drawForest . map (fmap simpLab . S.simplify) $ mweTrees
          forM_ mweTrees $ \mweTree -> do
            putStrLn . S.drawTree . S.mapFst simpLab . fmap show $ mweTree
  where
    simpLab = L.unpack . either S.cat S.orth . S.label
    showTree = R.drawTree . fmap simpLab . S.simplify
    forestFromXml xml = do
      nodesDAG <- S.mkDAG <$> S.readTop xml
      return $ S.forest S.chosen 0 nodesDAG


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
