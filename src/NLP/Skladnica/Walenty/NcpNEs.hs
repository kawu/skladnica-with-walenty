-- | A module responsible for extracting named entities from the National Corpus
-- of Polish.


module NLP.Skladnica.Walenty.NcpNEs
( nesInCorpus
) where


import           Data.Text       (Text)
import qualified Data.Text.Lazy  as L

-- import qualified Data.Named.Tree as NE
import qualified Text.NKJP.Named as NCP


-- | Extract NEs present in an NCP paragraph.
nesInCorpus :: FilePath -> IO [Text]
nesInCorpus corpusPath = do
  xs0 <- NCP.readCorpus [] corpusPath
  let nes = concatMap nesInPara
        [ fmap L.toStrict para
        | (_filePath, Just paras) <- xs0
        , para <- paras ]
  return (map NCP.orth nes)


-- | Extract NEs present in an NCP paragraph.
nesInPara :: NCP.Para Text -> [NCP.NE Text]
nesInPara = concatMap nesInSent . NCP.sentences


-- | Extract NEs present in an NCP paragraph.
nesInSent :: NCP.Sent Text -> [NCP.NE Text]
nesInSent = NCP.names
