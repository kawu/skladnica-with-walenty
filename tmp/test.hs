import NLP.Skladnica.Walenty (runTest)

main :: IO ()
main = do
  runTest
    "./data/skladnica/NKJP_1M_2004000055/morph_6-p/morph_6.50-s.xml"
    "./data/walenty.txt"
    "./data/walenty/expand.txt"
