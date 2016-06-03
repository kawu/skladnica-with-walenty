import qualified NLP.Skladnica.Walenty as W

main :: IO ()
main = do
  -- W.runTest
  W.fullTest

    "./data/skladnica/"

    -- "./data/walenty2.txt"
    "./data/walenty/verbs/walenty_2015_05_verbs_verified.txt"
    "./data/walenty/expand.txt"
