{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


-- | Representing, parsing, and rendering Skladnica syntactic trees
-- marked with MWEs.


module NLP.Skladnica.Walenty.MweTree
(
-- * Core
  MweTree
, MweNode (..)
, cleanUp

-- * Conversion
, OutTree
, fromOut

-- * Rendering
, XmlTree
, XmlForest
, rootToXml
, mweTreeXml
, renderXml
-- , renderXml'

-- * Utils
, outToXml
-- , outToXml'
) where


import           Control.Monad      (msum)

-- import qualified Data.ByteString    as BS
import qualified Data.Map.Strict    as M
import qualified Data.Set           as S
import qualified Data.Text          as T
-- import qualified Data.Text.Encoding as T
import qualified Data.Tree          as R

import qualified Text.HTML.TagSoup  as Tag
import qualified Text.XML.PolySoup  as Poly

import qualified NLP.Skladnica      as Skl
import qualified NLP.Walenty.Types  as W


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------


-- | Skladnica tree marked with MWEs.
type MweTree = R.Tree MweNode


-- | A node of `MweTree`.
data MweNode = MweNode
  { sklNode :: Skl.Edge Skl.Node Skl.IsHead
    -- ^ The underlying node in Skladnica tree
  , mweSet  :: S.Set Skl.NID
    -- ^ Set of identifiers of terminal leaves making up the MWE
    -- rooted in the current node; no MWE if the set is empty
  } deriving (Show, Eq, Ord)


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
          rootLabel {mweSet = S.empty}
      | otherwise = rootLabel


------------------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------------------


-- | What comes from the mapping.
type OutTree = R.Tree (Skl.Edge Skl.Node Skl.IsHead, S.Set Skl.NID)


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
rootToXml :: [(T.Text, T.Text)] -> MweTree -> XmlTree
rootToXml atts root = R.Node
  { R.rootLabel = Tag.TagOpen "tree" atts
  , R.subForest = [mweTreeXml root ]}


-- | Convert a `MweTree` to an XML tree.
mweTreeXml :: MweTree -> XmlTree
mweTreeXml root@R.Node{..} = R.Node
  { R.rootLabel = Tag.TagOpen nodeType atts
  , R.subForest =
      labelXml (Skl.label nodeLabel) ++
      mweForest ++
      map mweTreeXml subForest }
  where
    nodeLabel = Skl.nodeLabel . sklNode $ rootLabel
    nodeType = case Skl.label nodeLabel of
      Left _nonTerm -> "node"
      Right _term -> "leaf"
    nodeNid = T.pack . show . Skl.nid $ nodeLabel
    -- atts = [("nid", nodeNid), ("type", nodeType)]
    atts = [("nid", nodeNid)]
    mweForest = mweSetXml root . mweSet $ rootLabel


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
mweSetXml :: MweTree -> S.Set Skl.NID -> XmlForest
mweSetXml tree s
  | S.null s = []
  | otherwise = (:[]) $ R.Node
    { R.rootLabel = Tag.TagOpen "mwe" []
    , R.subForest = map (mweXml tree) (S.toList s) }


-- | Convert a MWE to an XML tree.
mweXml :: MweTree -> Skl.NID -> XmlTree
mweXml tree nid = leaf "lex"
  [ ("nid", T.pack (show nid))
  , ("base", maybe "???" id
             $ findBase nid tree) ]


-- | Find base form corresponding to the given node ID.
findBase :: Skl.NID -> MweTree -> Maybe T.Text
findBase i R.Node{..}
  | i == nodeNid = nodeBase
  | otherwise = msum $ map (findBase i) subForest
  where
    nodeNid = Skl.nid nodeLabel
    nodeLabel = Skl.nodeLabel . sklNode $ rootLabel
    nodeBase = case Skl.label nodeLabel of
      Right Skl.Term{..} -> Just base
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
  , R.subForest = [text val] }


-- | Textual leaf.
text :: T.Text -> XmlTree
text x = R.Node (Tag.TagText x) []


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


-- -- | Like `renderXml` but gives utf8 encoded bytestring.
-- renderXml' :: XmlTree -> BS.ByteString
-- renderXml' = Tag.renderTags . map (fmap T.encodeUtf8) . Poly.renderTree


------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------


-- | A function which combines several lower-level functions and produces an XML
-- tree (in textual form) from a MWE->Skladnica mapping output.
outToXml :: [(T.Text, T.Text)] -> OutTree -> T.Text
outToXml atts = renderXml . rootToXml atts . cleanUp . fromOut


-- -- | Like `outToXml` but gives utf8 encoded bytestring.
-- outToXml' :: OutTree -> BS.ByteString
-- outToXml' = renderXml' . mweTreeXml . cleanUp . fromOut
