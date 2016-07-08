import qualified NLP.Skladnica.Walenty as W

main :: IO ()
main = do
  -- W.runTest
  W.fullTest

    "./data/skladnica-small/"
    -- "./data/skladnica/"
    -- "./data/skladnica/NKJP_1M_7121900001/morph_17-p/"

    "./data/walenty/verbs/walenty_2015_05_verbs_verified.txt"
    "./data/walenty/expand.txt"
    "./data/sejf/SEJF-1.1-dlcf.dic"
    "./data/nkjp-small"
    -- "./data/nkjp-annex"
