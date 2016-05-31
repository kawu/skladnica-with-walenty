import NLP.Skladnica.Walenty (runTest)

main :: IO ()
main = do
  runTest
    -- -- <Bierzemy pod lupę najbliższych sąsiadów>
    -- "./data/skladnica/NKJP_1M_2004000055/morph_6-p/morph_6.50-s.xml"

    "./data/skladnica/"
    -- "./data/skladnica/NKJP_1M_1305000000631/morph_1-p"

    -- "./data/walenty2.txt"
    "./data/walenty/verbs/walenty_2015_05_verbs_verified.txt"
    "./data/walenty/expand.txt"
