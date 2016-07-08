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
, mweTreeXml
, renderXml
) where


import qualified Data.Set          as S
import qualified Data.Text         as T
import qualified Data.Tree         as R

import qualified Text.HTML.TagSoup as Tag
import qualified Text.XML.PolySoup as Poly

import qualified NLP.Skladnica     as Skl
import qualified NLP.Walenty.Types as W



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
-- XML Rendering
--------------------------------------------------------------------------------


-- | Local XML tree type.
type XmlTree = Poly.XmlTree T.Text

-- | Local XML forest type.
type XmlForest = Poly.XmlForest T.Text


-- | Convert a `MweTree` to an XML tree.
mweTreeXml :: MweTree -> XmlTree
mweTreeXml R.Node{..} = R.Node
  { R.rootLabel = Tag.TagOpen "node" [("nid", T.pack (show nid))]
  , R.subForest = map mweTreeXml subForest }
  where
    nid = Skl.nid . Skl.nodeLabel . sklNode $ rootLabel


-- | Render XML tree.
renderXml :: XmlTree -> T.Text
renderXml = Tag.renderTags . Poly.renderTree


-- -- | Convert a `Skl.Node` to an XML tree.
-- sklNodeXml :: Skl.Node -> XmlTree
-- sklNodeXml Skl.Node{..} = R.Node
--   { R.rootLabel = Tag.TagOpen "nid" [("nid", T.pack (show nid))]
--   , R.subForest = [] }
