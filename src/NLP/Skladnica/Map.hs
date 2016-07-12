{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


-- | A module responsible for identifying occurrences dictionary
-- entries in the Skladnica treebank.


module NLP.Skladnica.Map
( MapCfg (..)
, mapMWEs
) where


import           Control.Monad                 (forM_)

-- import qualified Data.ByteString               as BS
import           Data.Either                   (lefts)
import           Data.Maybe                    (mapMaybe)
import qualified Data.Set                      as S
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Tree                     as R

import qualified System.FilePath.Find          as F

import qualified NLP.Skladnica                 as Skl
import qualified NLP.Walenty                   as W

import qualified NLP.Skladnica.Walenty.Mapping as Mapping
import qualified NLP.Skladnica.Walenty.Prune   as P
import qualified NLP.Skladnica.Walenty.Search2 as Q
-- import qualified NLP.Skladnica.Walenty.Select  as Select
import qualified NLP.Skladnica.Walenty.MweTree as MWE
import qualified NLP.Skladnica.Walenty.NcpNEs  as NE
import qualified NLP.Skladnica.Walenty.Sejf    as Sejf


------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------


-- | Recursively retrieve all Xml files from the given directory.
getXmlFiles :: FilePath -> IO [FilePath]
getXmlFiles dir = do
  F.find F.always (F.fileName F.~~? "*.xml") dir


------------------------------------------------------------------------------
-- Grammar Extraction Tests
--------------------------------------------------------------------------------


-- | Mapping configuration.
data MapCfg = MapCfg
  { skladnicaDir :: FilePath
  , mayWalentyPath :: Maybe (FilePath, FilePath)
    -- ^ The first component is the Walenty file, the second one
    -- is the expansion file
  , maySejfPath :: Maybe FilePath
  , mayNcpPath :: Maybe FilePath
  } deriving (Show, Eq, Ord)


-- | Map individual MWEs on Składnica and print the resulting trees on stdin.
-- mapMWEs
--   :: FilePath -- ^ Skladnica directory
--   -> FilePath -- ^ Walenty file
--   -> FilePath -- ^ Walenty expansion file
--   -> FilePath -- ^ SEJF file
--   -> FilePath -- ^ NCP directory (for NEs)
--   -> IO ()
mapMWEs
  :: MapCfg
  -> IO ()
mapMWEs MapCfg{..} = do
  -- read *lexicalized* verbal entries from Walenty
  walenty <- case mayWalentyPath of
    Just paths -> uncurry readWalenty paths
    Nothing -> return []
  -- read SEJF dictionary
  sejf0 <- case maySejfPath of
    Just sejfPath -> Sejf.readSejf sejfPath
    Nothing -> return []
  let sejf = sejfPartition Sejf.orth sejf0
  -- read NCP-NEs dictionary
  nes0 <- nubOrd <$> case mayNcpPath of
    Just ncpPath -> NE.nesInCorpus ncpPath
    Nothing -> return []
  let nes = sejfPartition NE.orth nes0
  -- per each XML file...
  xmlFiles <- getXmlFiles skladnicaDir
  forM_ xmlFiles (procPath walenty sejf nes)
  where
    procPath walenty sejf0 nes0 skladnicaXML = do
      sklForest <- forestFromXml skladnicaXML
      let sentSet = case sklForest of
            sklTree : _ -> S.fromList $
              map Skl.orth (Mapping.terminals sklTree)
            _ -> S.empty
          sejf =
            [ entry
            | (entry, wordSet) <- sejf0
            , wordSet `S.isSubsetOf` sentSet ]
          nes =
            [ entry
            | (entry, wordSet) <- nes0
            , wordSet `S.isSubsetOf` sentSet ]
      let exprs1 = map ((,walentyInfo) . Mapping.querify) walenty
          exprs2 = map ((,sejfInfo) . Sejf.querify' Sejf.IgnoreCase) sejf
          exprs3 = map
            ( \ne ->
                ( Sejf.querifyOrth' Sejf.CaseSensitive $ NE.orth ne
                , nesInfo ne )
            ) nes
      forM_ sklForest $ \sklTree -> do
        let mweTree = Mapping.markSklTree (exprs1 ++ exprs2 ++ exprs3) sklTree
        T.putStrLn . MWE.renderXml $
          let path = drop (length skladnicaDir) skladnicaXML
          in  MWE.outToXml
              . MWE.Top mweTree
              $ M.fromList [("file", T.pack path)]
    walentyInfo = genericInfo "walenty"
    sejfInfo = genericInfo "sejf"
    genericInfo orig = MWE.MweInfo
      { MWE.origin = Just orig
      , MWE.mweTyp = Nothing
      , MWE.reading = Nothing }
    nesInfo NE.NE{..} = MWE.MweInfo
      { MWE.origin = Just "nkjp"
      , MWE.mweTyp = Just $ neType `T.append` case neSubType of
          Nothing -> ""
          Just subType -> "-" `T.append` subType
      , MWE.reading = Nothing }


------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------


-- readWalenty
--   :: FilePath -- ^ Walenty filePath
--   -> FilePath -- ^ Expansion file
--   -> IO ()
readWalenty walentyPath expansionPath = do
  expMap <- W.readExpMap expansionPath
  walenty <-
       S.toList . S.fromList
     . mapMaybe P.pruneVerb
     -- NEW 01/06/2016: additional pruning is needed to remove non-lexical
     -- constraints introduced during in the process of expansion
     . map (W.expandVerb expMap)
     -- the first pruning is needed before expansions so that unsignificant
     -- expansions are not treated as lexical constraints
     . mapMaybe P.pruneVerb
     . lefts
    <$> W.readWalenty walentyPath
  return walenty


-- | Extract skladnica trees from the given XML file.
forestFromXml :: FilePath -> IO [Skl.Tree Skl.Node Skl.IsHead]
forestFromXml xml = do
  nodesDAG <- Skl.mkDAG <$> Skl.readTop xml
  return $ Skl.forest Skl.chosen 0 nodesDAG


-- | Use `Sejf.partition` to determine component words and put them (under the
-- form of a set) on the second position of the corresponding list elements.
-- Preserve only these entries which have more than one component words.
sejfPartition :: (a -> T.Text) -> [a] -> [(a, S.Set T.Text)]
-- sejfPartition f = map $ \x -> (x, S.fromList . Sejf.partition . f $ x)
sejfPartition f xs =
  [ (x, wordSet)
  | x <- xs
  , let wordSet = S.fromList . Sejf.partition . f $ x
  , S.size wordSet > 1 ]


--------------------------------------------------
-- Misc
--------------------------------------------------


nubOrd :: (Ord a) => [a] -> [a]
nubOrd = S.toList . S.fromList
