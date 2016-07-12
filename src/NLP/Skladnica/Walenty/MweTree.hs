{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


-- | Representing, parsing, and rendering Skladnica syntactic trees
-- marked with MWEs.


module NLP.Skladnica.Walenty.MweTree
(
-- * Core
  MweTree
, MweNode (..)
, MweInfo (..)
, Reading (..)
, cleanUp
, emboss
-- , embossTop

-- * Top-level tree
, Top (..)
, modifyRoot

-- * Conversion
, InTree
, OutTree
, fromOut

-- * Rendering
, XmlTree
, XmlForest
, rootToXml
, mweTreeXml
, renderXml

-- * Parsing
, readTop
, parseTop

-- * Utils
, outToXml
, parseAndPrint
) where


import           Control.Applicative           (optional, (<|>))
import qualified Control.Arrow                 as Arr
import qualified Control.Monad                 as Monad
import           Control.Monad                 (msum)

import           Data.Maybe                    (catMaybes)
import qualified Data.Foldable                 as F
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy                as L
import qualified Data.Text.Lazy.IO             as L
import qualified Data.Tree                     as R

import qualified Text.HTML.TagSoup             as Tag
import           Text.XML.PolySoup             hiding (P, Q, XmlForest, XmlTree)
import qualified Text.XML.PolySoup             as Poly

import qualified NLP.Skladnica                 as Skl
import qualified NLP.Skladnica.Walenty.Search2 as Q
import qualified NLP.Walenty.Types             as W


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------


-- -- | Skladnica tree marked with MWEs.
-- data MweTree = MweTree
--   { mweTree :: R.Tree MweNode
--     -- ^ The MWE tree itself
--   , mweTreeAtts :: M.Map T.Text T.Text
--     -- ^ Top-level attributes of the tree
--   } deriving (Show, Eq)


-- | Skladnica tree marked with MWEs.
type MweTree = R.Tree MweNode


-- | Top-level tree with additional attributes.
data Top t = Top
  { topRoot :: t
    -- ^ The underlying tree
  , topAtts :: M.Map T.Text T.Text
    -- ^ Top-level attributes of the tree
  }

-- | Modify the underlying rose tree.
modifyRoot :: (a -> b) -> Top a -> Top b
modifyRoot f top@Top{..} = top {topRoot = f topRoot}


-- | A node of `MweTree`.
data MweNode = MweNode
  { sklNode :: Skl.Edge Skl.Node Skl.IsHead
    -- ^ The underlying node in Skladnica tree
  -- , mweSet  :: S.Set Skl.NID
  , mweSet  :: Maybe (Q.Occur Skl.NID MweInfo)
    -- ^ Set of identifiers of terminal leaves making up the MWE
    -- rooted in the current node; no MWE if the set is empty
  } deriving (Show, Eq, Ord)


-- | Information about the occurrence of a MWE.
data MweInfo = MweInfo
  { origin  :: Maybe T.Text
    -- ^ Origin of the MWE
  , mweTyp  :: Maybe T.Text
    -- ^ Type of the MWE
  , reading :: Maybe Reading
    -- ^ Reading of the MWE
  } deriving (Show, Eq, Ord)


-- | Reading of a MWE.
data Reading = Idiomatic | Compositional | Error
  deriving (Show, Eq, Ord)



-- | Retrieve ID of the root node.
getRootID :: MweTree -> Skl.NID
getRootID =
  Skl.nid . Skl.nodeLabel . sklNode . R.rootLabel


-- | Mark the given node as a head.
markAsHead :: MweNode -> MweNode
markAsHead n =
  let newNode = Skl.modifyEdge
        (const Skl.HeadYes)
        (sklNode n)
  in  n {sklNode = newNode}


-- | Remove outer MWE annotations if identical, directly embedded MWE
-- annotations exist. In other words, if a parent node `n` has the same MWE
-- annotation as its child node `m`, we clean it (set to the empty set).
cleanUp :: MweTree -> MweTree
cleanUp R.Node{..} = R.Node
  { R.rootLabel = newRootLabel
  , R.subForest = map cleanUp subForest}
  where
    childrenMweSets = map (mweSet . R.rootLabel) subForest
    newRootLabel
      | mweSet rootLabel `elem` childrenMweSets =
          -- rootLabel {mweSet = S.empty}
          rootLabel {mweSet = Nothing}
      | otherwise = rootLabel


-- | Mark the paths leading from MWE roots to the corresponding leaves, by
-- changing the values of `Skl.IsHead` to `Skl.HeadYes`.  Emboss only the
-- MWEs whose meta-information satisfy the given predicate.
emboss :: (MweInfo -> Bool) -> MweTree -> MweTree
emboss mwePred R.Node{..} = R.Node
  { R.rootLabel = rootLabel
  , R.subForest = mark (map (emboss mwePred) subForest) }
  where
    mark = maybe id id $ do
      oc <- mweSet rootLabel
      Monad.guard . mwePred $ Q.exprInfo oc
      return $ map $ fst . go (Q.markedSet oc)
    go mweIDs t
      | or found || getRootID t `S.member` mweIDs =
          ( t { R.rootLabel = markAsHead (R.rootLabel t)
              , R.subForest = children }
          , True )
      | otherwise = (t, False)
      where
        pairs = map (go mweIDs) (R.subForest t)
        (children, found) = unzip pairs


-- -- | Top-level `emboss`.
-- embossTop :: (MweInfo -> Bool) -> Top MweTree -> Top MweTree
-- embossTop p = modifyRoot (emboss p)


------------------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------------------


-- | Skladnica input tree.
type InTree = R.Tree (Skl.Edge Skl.Node Skl.IsHead)


-- | Skladnica output (i.e., with MWEs) tree.
type OutTree = R.Tree
  ( Skl.Edge Skl.Node Skl.IsHead
  , Maybe (Q.Occur Skl.NID MweInfo) )


-- -- | What comes from the mapping.
-- -- type OutTree = R.Tree (Skl.Edge Skl.Node Skl.IsHead, S.Set Skl.NID)
-- type OutTree = Mapping.OutTree


-- | Create `MweTree` from the tree resulting from MWEs->Skladnica mapping.
fromOut :: OutTree -> MweTree
fromOut = fmap $ uncurry MweNode


------------------------------------------------------------------------------
-- Conversion to XML
--------------------------------------------------------------------------------


-- | Local XML tree type.
type XmlTree = Poly.XmlTree T.Text

-- | Local XML forest type.
type XmlForest = Poly.XmlForest T.Text


-- | Top-level conversion from a `MweTree` to an XML tree.
-- The function takes as argument a list of attribute/value
-- pairs that will be assigned to the top-level <tree> node.
-- rootToXml :: [(T.Text, T.Text)] -> MweTree -> XmlTree
rootToXml :: Top MweTree -> XmlTree
rootToXml Top{..} = R.Node
  { R.rootLabel = Tag.TagOpen "tree" (M.toList topAtts)
  , R.subForest = [mweTreeXml topRoot ]}


-- | Convert a `MweTree` to an XML tree.
mweTreeXml :: MweTree -> XmlTree
mweTreeXml root@R.Node{..} = R.Node
  { R.rootLabel = Tag.TagOpen nodeType atts
  , R.subForest =
      labelXml (Skl.label nodeLabel) ++
      mweForest ++
      case subForest of
        [] -> []
        _ -> [childrenTree]
  }
  where
    nodeLabel = Skl.nodeLabel . sklNode $ rootLabel
    nodeType = case Skl.label nodeLabel of
      Left _nonTerm -> "node"
      Right _term -> "leaf"
    atts = [("nid", nodeNid), ("head", nodeHead)]
    nodeNid = T.pack . show . Skl.nid $ nodeLabel
    nodeHead = case Skl.edgeLabel (sklNode rootLabel) of
      Skl.HeadYes -> "yes"
      Skl.HeadNo -> "no"
    -- mweForest = mweSetXml root . mweSet $ rootLabel
    mweForest = case mweSet rootLabel of
      Nothing -> []
      Just oc -> mweSetXml root oc
    childrenTree = R.Node
      { R.rootLabel = Tag.TagOpen "children" []
      , R.subForest = map mweTreeXml subForest }


-- | Convert a node label to an XML forest containing additional information
-- about the specific node.
labelXml :: Skl.Label -> XmlForest
labelXml (Right Skl.Term{..}) =
  [ atom' "orth" orth
  , atom' "base" base
  , atom' "tag" tag ]
labelXml (Left Skl.NonTerm{..}) =
  [ atom' "cat" cat
  , leaf "morph" (M.toList morph) ]
--   , R.Node (Tag.TagOpen "morph" [])
--       [atom' attr val | (attr, val) <- M.toList morph]


-- | Convert a set of MWE identifiers to an XML forest.
mweSetXml :: MweTree -> Q.Occur Skl.NID MweInfo -> XmlForest
mweSetXml tree occur
  | S.null (Q.markedSet occur) = []
  | otherwise = (:[]) $ R.Node
    { R.rootLabel = Tag.TagOpen "mwe" atts
    , R.subForest = map (mweXml tree) (S.toList $ Q.markedSet occur) }
  where
    atts = catMaybes
      [ ("origin",) <$> origin (Q.exprInfo occur)
      , ("type",) <$> mweTyp (Q.exprInfo occur)
      , Just . ("reading",) . showReading $ reading (Q.exprInfo occur) ]
    showReading (Just Idiomatic) = "idiomatic"
    showReading (Just Compositional) = "compositional"
    showReading (Just Error) = "error"
    showReading Nothing = "unknown"
-- mweSetXml :: MweTree -> S.Set Skl.NID -> XmlForest
-- mweSetXml tree s
--   | S.null s = []
--   | otherwise = (:[]) $ R.Node
--     { R.rootLabel = Tag.TagOpen "mwe" []
--     , R.subForest = map (mweXml tree) (S.toList s) }


-- | Convert a MWE to an XML tree.
mweXml :: MweTree -> Skl.NID -> XmlTree
mweXml tree nid = leaf "lex"
  [ ("nid", T.pack (show nid))
  , ("orth", maybe "???" id
             $ findTerm Skl.orth nid tree) ]


-- | Find base form corresponding to the given node ID.
findTerm :: (Skl.Term -> T.Text) -> Skl.NID -> MweTree -> Maybe T.Text
findTerm p i R.Node{..}
  | i == nid = value
  | otherwise = msum $ map (findTerm p i) subForest
  where
    nid = Skl.nid label
    label = Skl.nodeLabel . sklNode $ rootLabel
    value = case Skl.label label of
      Right term -> Just (p term)
      _ -> Nothing


-- | Leaf tree with the given list of attributes.
leaf :: T.Text -> [(T.Text, T.Text)] -> XmlTree
leaf name atts = R.Node (Tag.TagOpen name atts) []


-- -- | Leaf tree with an atomic value.
-- atom :: T.Text -> T.Text -> XmlTree
-- atom name val = leaf name [("val", val)]


-- | Alternative atomic value where the value is stored as a textual leaf.
atom' :: T.Text -> T.Text -> XmlTree
atom' name val = R.Node
  { R.rootLabel = Tag.TagOpen name []
  , R.subForest = [textLeaf val] }


-- | Textual leaf.
textLeaf :: T.Text -> XmlTree
textLeaf x = R.Node (Tag.TagText x) []


-- -- | Convert a `Skl.Node` to an XML tree.
-- sklNodeXml :: Skl.Node -> XmlTree
-- sklNodeXml Skl.Node{..} = R.Node
--   { R.rootLabel = Tag.TagOpen "nid" [("nid", T.pack (show nid))]
--   , R.subForest = [] }


------------------------------------------------------------------------------
-- XML Rendering
--------------------------------------------------------------------------------


-- | Render XML tree.
renderXml :: XmlTree -> T.Text
renderXml = Tag.renderTags . Poly.renderTree


------------------------------------------------------------------------------
-- Parsing from XML
--------------------------------------------------------------------------------


-- | Parsing predicates.
type P a = Poly.P (Poly.XmlTree L.Text) a
type Q a = Poly.Q (Poly.XmlTree L.Text) a


-- | Top-level parser
topP :: P [Top MweTree]
topP = every' topTreeQ


-- | Tree parser
topTreeQ :: Q (Top MweTree)
topTreeQ = (named "tree" *> atts) `join`
  \as -> do
    t <- first treeQ
    let toStrict = map
          ( Arr.first L.toStrict
          . Arr.second L.toStrict )
    return $ Top t (M.fromList $ toStrict as)


-- | (Sub)tree parser
treeQ :: Q MweTree
treeQ = nodeQ <|> leafQ


-- | Internal node parser
nodeQ :: Q MweTree
nodeQ =
  (named "node" *> ((,) <$> attr "nid" <*> attr "head")) `join`
    \(nidStr, headStr) -> do
      cat <- first catQ
      morph <- first morphQ
      children <- first childrenQ
      mwe <- optional $ first mweQ
      let edge = Skl.Edge
            { Skl.edgeLabel = edgeLabel
            , Skl.nodeLabel = nodeLabel }
          edgeLabel = case headStr of
            "yes" -> Skl.HeadYes
            _ -> Skl.HeadNo
          nodeLabel = Skl.Node
            { Skl.nid = read (L.unpack nidStr)
            , Skl.chosen = True -- whatever...
            , Skl.label = Left $ Skl.NonTerm cat morph
            , Skl.children = [] -- whatever...
            }
      return $ R.Node
        { R.rootLabel = MweNode
          { sklNode = edge
          , mweSet = mwe }
        , R.subForest = children }


-- | MWE set parser.
-- mweQ :: Q (S.Set Skl.NID)
mweQ :: Q (Q.Occur Skl.NID MweInfo)
-- mweQ = S.fromList <$> named "mwe" `joinR` every' lexQ
mweQ = (named "mwe" *> mweInfoQ) `join` \mweInfo -> do
  lexs <- every' lexQ
  return $ Q.Occur
    { Q.markedSet = S.fromList lexs
    , Q.exprInfo = mweInfo }
  where
    mweInfoQ = MweInfo
      <$> optional (L.toStrict <$> attr "origin")
      <*> optional (L.toStrict <$> attr "type")
      <*> (readReading <$> attr "reading")
    readReading "idiomatic" = Just Idiomatic
    readReading "compositional" = Just Compositional
    readReading "error" = Just Error
    readReading _ = Nothing


-- | Single MWE parser.
lexQ :: Q Skl.NID
lexQ = node $ named "lex" *> (read . L.unpack <$> attr "nid")


-- | Category parser
catQ :: Q T.Text
-- catQ = named "cat" `joinR` first (node text)
catQ = atomQ "cat"


-- | Morphosyntactic parser
morphQ :: Q (M.Map T.Text T.Text)
morphQ =
  let toStrict = Arr.first L.toStrict . Arr.second L.toStrict
  in  M.fromList . fmap toStrict <$> node (named "morph" *> atts)


-- | Children parser
childrenQ :: Q [MweTree]
childrenQ = named "children" `joinR` every' treeQ


-- | Leaf node parser
leafQ :: Q MweTree
leafQ =
  (named "leaf" *> ((,) <$> attr "nid" <*> attr "head")) `join`
  \(nidStr, headStr) -> do
    orth <- first $ atomQ "orth"
    base <- first $ atomQ "base"
    tag <- first $ atomQ "tag"
    let edge = Skl.Edge
          { Skl.edgeLabel = edgeLabel
          , Skl.nodeLabel = nodeLabel }
        edgeLabel = case headStr of
          "yes" -> Skl.HeadYes
          _ -> Skl.HeadNo
        nodeLabel = Skl.Node
          { Skl.nid = read (L.unpack nidStr)
          , Skl.chosen = True -- whatever...
          , Skl.label = Right $ Skl.Term orth base tag
          , Skl.children = [] -- whatever...
          }
    return $ R.Node
      { R.rootLabel = MweNode {sklNode = edge, mweSet = Nothing}
      , R.subForest = [] }


-- | Atomic value parser
atomQ :: L.Text -> Q T.Text
atomQ name = L.toStrict <$> named name `joinR` first (node text)


-- | Parse an XML string into a sequence of `MweTree`s.
parseTop :: L.Text -> [Top MweTree]
parseTop =
    F.concat . evalP topP . parseForest . Tag.parseTags


-- | Read an XML file into a sequence of `MweTree`s.
readTop :: FilePath -> IO [Top MweTree]
readTop path = parseTop <$> L.readFile path


------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------


-- | Functions which just parses and prints the given XML file
-- with MWE-marked trees.  It takes a single argument which
-- allows to modify the individual MWE trees.
parseAndPrint :: (Top MweTree -> Top MweTree) -> FilePath -> IO ()
parseAndPrint f path = do
  mweTrees <- map f <$> readTop path
  mapM_ (T.putStrLn . renderXml . rootToXml) mweTrees


-- | A function which combines several lower-level functions and produces an XML
-- tree (in textual form) from a MWE->Skladnica mapping output.
outToXml :: Top OutTree -> XmlTree
outToXml = rootToXml . modifyRoot (cleanUp . fromOut)
-- outToXml :: [(T.Text, T.Text)] -> OutTree -> XmlTree
-- outToXml atts = rootToXml atts . cleanUp . fromOut
