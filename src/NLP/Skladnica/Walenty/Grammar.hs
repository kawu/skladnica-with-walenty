{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}


-- | Provisional grammar extraction module.


module NLP.Skladnica.Walenty.Grammar
( ET
, Status (..)
, extractGrammar
, topShatter
, prepTree
) where


import           Control.Monad                (guard)
import qualified Control.Monad.State.Strict   as E

import qualified Data.Map.Strict              as M
import           Data.Maybe                   (isJust)
import qualified Data.Set                     as S
import           Data.Text                    (Text)
import qualified Data.Tree                    as R

-- NLP.Partage.DAG: for Ord R.Tree instance
import           NLP.Partage.DAG              ()
import qualified NLP.Partage.Tree.Other       as O

import qualified NLP.Skladnica                as S
import qualified NLP.Walenty.Types            as W

import qualified NLP.Skladnica.Walenty.Search as Q


-------------------------------------------------
-- Grammar extraction -- determining the status
-------------------------------------------------


-- | A status of the node w.r.t. to its parent.
data Status
  = Trunk -- ^ The node is a part of the trunk (and should be thus
          -- connected to its parent)
  | Modif -- ^ Optional modifier of its parent (should be thus
          -- modeled by adjunction)
  | Arg   -- ^ Obligatory argument (open slot) of its parent
          -- (to be modeled through substitution)
  deriving (Show, Eq, Ord)


-- | A function which decides, based on the labels of the path
-- from the given node up to the root, and the corresponding
-- `IsHead` markers, what is the `Status` of the current node.
status
  :: [(S.Label, S.IsHead)] -- ^ Labels on the path from the node to the root
  -> Status
status xs = case xs of
  [] -> Trunk -- not really important what we return in this case
  (curLab, curHead) : (parLab, parHead) : _
    | curHead == S.HeadYes &&
      parHead == S.HeadYes -> Trunk
    | parLab `is` "fw" &&
      hasAttrIn parLab "tfw"
        ["sentp(że)"]    -> Trunk
    | parLab `is` "fw"   -> Arg
    | parLab `is` "fl"   -> Modif
    | curHead == S.HeadYes -> Trunk
    | curLab `is` "fno" &&
      parLab `is` "fpm"  -> Arg
    | curLab `is` "zdanie" &&
      parLab `is` "fzd"  -> Arg
    | otherwise          -> Modif
  (curLab, curHead) : []
    | curHead == S.HeadYes -> Trunk
    | otherwise          -> Modif
  _ -> error $ "unhandled case in `status`: " ++ show xs
  where
    is (Left S.NonTerm{..}) x = x == cat
    is _ _ = False
    hasAttrIn (Left S.NonTerm{..}) attr vals = isJust $ do
      val <- M.lookup attr morph
      guard $ val `elem` vals
    hasAttrIn _ _ _ = False


-- | Determine the status of internal tree nodes.
statRoot
  :: S.Tree S.Label S.IsHead           -- ^ Input tree
  -> S.Tree (S.Label, Status) S.IsHead -- ^ Status-labeled tree
statRoot = statTree []


-- | Determine the status of the internal tree nodes.
statTree
  :: [(S.Label, S.IsHead)]           -- ^ Trace from the current node to the root
  -> S.Tree S.Label S.IsHead           -- ^ Current node (tree)
  -> S.Tree (S.Label, Status) S.IsHead -- ^ Status-labeled current node (tree)
statTree trace t
   | null (S.subForest t) = S.Tree
       -- whatever the leaf, it must be in a trunk
       { S.rootLabel = (S.rootLabel t, Trunk)
       , S.subForest = [] }
   | otherwise =
       let (subTrees0, areHeads0) = unzip (S.subForest t)
           areHeads =
             -- WARNING! IMPORTANT: a backup strategy which
             -- handles the case when none of the children
             -- is marked as the head.
             if S.HeadYes `elem` areHeads0
               then areHeads0
               -- the first child is arbitrarily chosen as the head
               else S.HeadYes : repeat S.HeadNo
           subTrees = flip map (zip subTrees0 areHeads) $
             \(subTree, isHead) ->
               statTree
                 ((S.rootLabel subTree, isHead) : trace)
                 subTree
        in S.Tree { S.rootLabel =
                    ( S.rootLabel t
                    , status trace )
                , S.subForest = zip subTrees areHeads }


-- | Prepare the tree for grammar extraction:
--
--   * Use `statRoot` to obtain the status of nodes
--   * `purge` redundant (e.g., "fw", "fl" and "ff") nodes
--
prepTree
  :: S.Tree S.Label S.IsHead
  -> R.Tree (S.Label, Status)
prepTree
  = check
  . S.purge (useless . fst)
  . S.simplify
  . statRoot
  where
    check [x] = x
    check xs  = error $ "prepTree: purge left " ++ show xs
    useless (Left S.NonTerm{..}) =
      cat `elem` ["fw", "fl", "ff"]
    useless _ = False


-------------------------------------------------
-- Neighbors
-------------------------------------------------


-- | Neighboring label.
data Neighbor x
  = Parent x -- ^ The closest neighbor is the parent
  | Sister x -- ^ The closest neighbor is the sister (or the node itself)
  | None     -- ^ No neighbor
  deriving (Show, Eq, Ord)


-- | For a given list of labels, determine the left-neighbouring
-- trunk non-terminals for the individual input elements.
onLeft
  :: Neighbor S.NonTerm  -- ^ Last trunk non-terminal on the left
  -> [(S.Label, Status)] -- ^ The list of labels and their statuses
  -> [Neighbor S.NonTerm]
onLeft _ [] = []
onLeft prev ((curr, stat) : rest) =
  case (curr, stat) of
    (Left x, Trunk) -> Sister x : onLeft (Sister x) rest
    (_, Modif)      -> prev : onLeft prev rest
    _               -> None : onLeft None rest


-------------------------------------------------
-- Grammar extraction
-------------------------------------------------


-- -- | Again, a standalone Ord instance for rose trees...
-- deriving instance Ord a => Ord (R.Tree a)


-- | A TAG elementary tree.
type ET = O.Tree Text Text


-- | Store the ET in the underlying set.
store :: ET -> E.State (S.Set ET) ()
store = E.modify' . S.insert


-- | Really, the top-level grammar extraction method.
extractGrammar :: Q.SklTree -> S.Set ET
extractGrammar = cleanGrammar . topShatter . prepTree . S.mapFst S.label


-- | Top-level `shatter`
topShatter :: R.Tree (S.Label, Status) -> S.Set ET
topShatter =
  let doit x = shatter x >>= store
  in  flip E.execState S.empty . doit


-- | Shatter a given parsed tree into the set of the component
-- elementary trees.
shatter :: R.Tree (S.Label, Status) -> E.State (S.Set ET) ET
shatter r
  | null (R.subForest r) = do
      let x = getTerm' r
      return $ R.Node (O.Term x) []
  | otherwise = do
      let rootNT = labelNT . fst $ R.rootLabel r
          childLabs = map R.rootLabel $ R.subForest r
          left = onLeft (Parent rootNT) childLabs
      children' <- fst <$> go rootNT left (R.subForest r)
      return $ R.Node (O.NonTerm $ S.cat rootNT) children'
  where
    go rootNT [] [] = return ([], Parent rootNT)
    go rootNT (left:lefts) (child:children) = do
      let (childLab, childStat) = R.rootLabel child
      (children', right) <- go rootNT lefts children
      child' <- shatter child
      case (childLab, childStat) of
        (Left x, Trunk)  -> return (child':children', Sister x)
        (Right _, Trunk) -> return (child':children', None)
        (Left x, Arg) -> do
          store child'
          let childNT = O.NonTerm $ S.cat x
          return (R.Node childNT [] : children', None)
        (Right _, Arg) -> error "shatter.go: obligatory terminal argument!?"
        (_, Modif) -> case (left, right) of
          (Parent x, _) -> (children', right) <$ store (child' `leftMod` x)
          (_, Parent y) -> (children', right) <$ store (child' `rightMod` y)
          (Sister x, _) -> (children', right) <$ store (child' `rightMod` x)
          (_, Sister y) -> (children', right) <$ store (child' `leftMod` y)
          (None, None) -> do
            store (child' `leftMod` rootNT)
            let newChild = R.Node (O.NonTerm $ S.cat rootNT) children'
            return ([newChild], Sister rootNT)
    go _ _ _ = error "shatter.go: different lengths of the input lists"


-- | Construct a left modifier from a given (initial,
-- but this is not checked!) ET.
leftMod :: ET -> S.NonTerm -> ET
leftMod t nonTerm = R.Node (O.NonTerm x)
  [ t
  , R.Node (O.Foot x) [] ]
  where x = S.cat nonTerm


-- | Construct a right modifier from a given (initial,
-- but this is not checked!) ET.
rightMod :: ET -> S.NonTerm -> ET
rightMod t nonTerm = R.Node (O.NonTerm x)
  [ R.Node (O.Foot x) []
  , t ]
  where x = S.cat nonTerm


-------------------------------------------------
-- Grammar clean-up
-------------------------------------------------


-- | Perform cleaning procedures on the grammar.
-- Notably:
--
--   * Remove redundant non-terminal repetitions
cleanGrammar :: S.Set ET -> S.Set ET
cleanGrammar =
  S.fromList . map cleanET . S.toList
  where
    cleanET tree = case R.subForest tree of
      [child0] ->
        let child = cleanET child0
        in if R.rootLabel tree == R.rootLabel child
           then child
           else tree {R.subForest = [child]}
      xs -> tree {R.subForest = map cleanET xs}


-------------------------------------------------
-- Utils
-------------------------------------------------


-- -- -- | Obtain non-terminal from ET's root.
-- -- getNT :: ET -> L.Text
-- -- getNT t =
-- --   case R.rootLabel t of
-- --     O.NonTerm (Just x) -> x
-- --     _ -> error "getNT: invalid input ET"
--
--
-- -- | Obtain non-terminal from source tree's root.
-- getNT' :: Tree Node b -> L.Text
-- getNT' t =
--   case label (rootLabel t) of
--     Left (NonTerm{..}) -> cat
--     _ -> error "getNT': invalid source tree"


-- | Extract the non-terminal from the label (raise an error if not possible).
labelNT :: S.Label -> S.NonTerm
labelNT x = case x of
  Left x -> x
  _ -> error "labelNT: not a non-terminal"


-- -- -- | Obtain non-terminal from source tree's root.
-- -- getTerm' :: Tree Node b -> L.Text
-- -- getTerm' t =
-- --   case label (rootLabel t) of
-- --     Right (Term{..}) -> base
-- --     _ -> error "getT': invalid source tree"


-- | Obtain non-terminal from source tree's root.
getTerm' :: R.Tree (S.Label, a) -> Text
getTerm' l =
  case fst (R.rootLabel l) of
    Right (S.Term{..}) -> base
    _ -> error "getT': invalid source tree"


-- -- | Print the chosen (simplified) tree represented in the DAG.
-- printExtracted :: DAG -> IO ()
-- printExtracted dag =
--   let tree = forest chosen 0 dag !! 0
--       ets  = prepTree $ mapFst label tree
--       lab (Left x) = cat x
--       lab (Right t) = orth t
--    in putStrLn . R.drawTree . fmap (show . Arr.first lab) $ ets
--
--
-- -- | Print the chosen (simplified) tree represented in the DAG.
-- printShattered :: DAG -> IO ()
-- printShattered dag =
--   let tree = forest chosen 0 dag !! 0
--       ets  = topShatter . prepTree $ mapFst label tree
--    in putStrLn
--         . R.drawForest
--         . map (fmap show)
--         . S.toList
--         $ ets
