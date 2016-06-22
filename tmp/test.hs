import qualified NLP.Skladnica.Walenty as W

main :: IO ()
main = do
  -- W.runTest
  W.fullTest

    "./data/skladnica-medium/"

    -- "./data/walenty2.txt"
    "./data/walenty/verbs/walenty_2015_05_verbs_verified.txt"
    "./data/walenty/expand.txt"
    "./data/sejf/SEJF-1.1-dlcf.dic"
