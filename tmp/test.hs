import qualified NLP.Skladnica.Walenty as W

main :: IO ()
main = do
  -- W.runTest
  W.runExtraction
    -- -- <Bierzemy pod lupę najbliższych sąsiadów>
    -- "./data/skladnica/NKJP_1M_2004000055/morph_6-p/morph_6.50-s.xml"

    -- "./data/skladnica/"
    "./data/skladnica-tmp/"

    -- "./data/walenty2.txt"
    "./data/walenty/verbs/walenty_2015_05_verbs_verified.txt"
    "./data/walenty/expand.txt"
