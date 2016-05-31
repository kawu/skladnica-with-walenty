import NLP.Skladnica.Walenty (runTest)

main :: IO ()
main = do
  runTest
    -- -- <Bierzemy pod lupę najbliższych sąsiadów>
    -- "./data/skladnica/NKJP_1M_2004000055/morph_6-p/morph_6.50-s.xml"

    -- <Ponad 1600 osób wzięło wczoraj udział w 24. maratonie ulicznym we Wrocławiu.>
    "./data/skladnica-tmp"

    "./data/walenty.txt"
    "./data/walenty/expand.txt"
