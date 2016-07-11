{-# LANGUAGE RecordWildCards   #-}


-- | A module responsible for extracting named entities from the National Corpus
-- of Polish.


module NLP.Skladnica.Walenty.NcpNEs
( NE (..)
, nesInCorpus
) where


import           Data.Text       (Text)
import qualified Data.Text.Lazy  as L

-- import qualified Data.Named.Tree as NE
import qualified Text.NKJP.Named as NCP


-- | NE from NCP.
data NE = NE
  { orth :: Text
    -- ^ Orthographic form
  , neType :: Text
    -- ^ Type of NE
  , neSubType :: Maybe Text
    -- ^ Type of NE
  } deriving (Show, Eq, Ord)


-- | Extract NEs present in an NCP paragraph.
nesInCorpus :: FilePath -> IO [NE]
nesInCorpus corpusPath = do
  xs0 <- NCP.readCorpus [] corpusPath
  let nes = concatMap nesInPara
        [ fmap L.toStrict para
        | (_filePath, Just paras) <- xs0
        , para <- paras ]
  -- return (map NCP.orth nes)
  return [ force $ NE orth neType subType
         | NCP.NE{..} <- nes ]
  where
    -- dirty, dirty trick...
    force x = length (show x) `seq` x


-- | Extract NEs present in an NCP paragraph.
nesInPara :: NCP.Para Text -> [NCP.NE Text]
nesInPara = concatMap nesInSent . NCP.sentences


-- | Extract NEs present in an NCP paragraph.
nesInSent :: NCP.Sent Text -> [NCP.NE Text]
nesInSent = NCP.names
