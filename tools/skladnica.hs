{-# LANGUAGE RecordWildCards #-}


-- import           Data.Monoid (mempty)
import           Options.Applicative
import qualified Data.Char as C


-- -- import qualified NLP.Partage4Xmg.Automat as A
-- import qualified NLP.Partage4Xmg.Build as B
-- import qualified NLP.Partage4Xmg.Parse as P
-- import qualified NLP.Partage4Xmg.ParseLex as L
-- import qualified NLP.Partage4Xmg.Gen as G
-- -- import qualified NLP.Partage4Xmg.GenLex as GL
-- import qualified NLP.Partage4Xmg.Stats as S
-- import qualified NLP.Partage4Xmg.Select as S


import qualified NLP.Skladnica.Extract as E


--------------------------------------------------
-- Global Options
--------------------------------------------------


data Command
    = Map E.MapCfg
    -- ^ MWE -> Skladnica mapping

--     | Parse FilePath
--     -- ^ Only parse and show the input grammar


-- parseCompression :: Monad m => String -> m B.Compress
-- parseCompression s = return $ case map C.toLower s of
--     'a':_       -> B.Auto    -- Automaton
--     't':_       -> B.Trie    -- Trie
--     'l':_       -> B.List    -- List
--     's':_       -> B.SetAuto -- Set of automata
--     'x':_       -> B.SetTrie -- Set of tries
--     _           -> B.Auto


mapCfgParser :: Parser E.MapCfg
mapCfgParser = E.MapCfg
  <$> strOption
        ( long "skladnica"
       <> short 's'
       <> metavar "DIR"
       <> help "Skladnica directory" )
  <*> ( joinWal
        <$> ( optional . strOption )
              ( long "walenty"
             <> short 'w'
             <> metavar "FILE"
             <> help "Walenty file" )
        <*> ( optional . strOption )
              ( long "walenty-expansion"
             <> short 'e'
             <> metavar "FILE"
             <> help "Walenty expansion file" )
        )
  <*> ( optional . strOption )
        ( long "sejf"
       <> short 'f'
       <> metavar "FILE"
       <> help "Sejf dictionary file" )
  <*> ( optional . strOption )
        ( long "ncp"
       <> short 'n'
       <> metavar "DIR"
       <> help "NCP directory" )
  where
    joinWal m1 m2 = (,) <$> m1 <*> m2


-- buildCfgParser :: Parser B.BuildCfg
-- buildCfgParser = B.BuildCfg
--   <$> option
--           ( str >>= parseCompression )
--           ( metavar "COMPRESSION-METHOD"
--          <> value B.Auto
--          <> long "compression-method"
--          <> short 'c' )
--   <*> (not <$> switch
--           ( long "no-subtree-sharing"
--          <> short 'n' ))
-- 
-- 
-- 
-- buildOptions :: Parser (B.BuildData, B.BuildCfg)
-- buildOptions = (,)
--     <$> buildDataParser
--     <*> buildCfgParser
-- 
-- 
-- --------------------------------------------------
-- -- Parse options
-- --------------------------------------------------
-- 
-- 
-- parseOptions :: Parser Command
-- parseOptions = Parse
--   <$> strOption
--      ( long "grammar"
--     <> short 'g'
--     <> metavar "FILE"
--     <> help "Grammar .xml file" )
-- 
-- 
-- --------------------------------------------------
-- -- Lexicon options
-- --------------------------------------------------
-- 
-- 
-- lexicOptions :: Parser Command
-- lexicOptions = Lexicon
--   <$> strOption
--      ( long "lexicon"
--     <> short 'l'
--     <> metavar "FILE"
--     <> help "Lexicon .xml file" )
-- 
-- 
-- --------------------------------------------------
-- -- Generation options
-- --------------------------------------------------
-- 
-- 
-- genOptions :: Parser Command
-- genOptions = Gen
--     <$> buildDataParser
--     <*> option
--             auto
--             ( metavar "MAX-SIZE"
--            <> value 5
--            <> long "max-size"
--            <> short 'm' )
-- 
-- 
-- --------------------------------------------------
-- -- Generation/parsing options
-- --------------------------------------------------
-- 
-- 
-- genRandOptions :: Parser Command
-- genRandOptions = GenRand
--     <$> buildDataParser
--     <*> (G.GenConf
--         <$> option
--                 auto
--                 ( metavar "MAX-SIZE"
--                <> value 5
--                <> long "max-size"
--                <> short 'm' )
--         <*> option
--                 auto
--                 ( metavar "ADJOIN-PROB"
--                <> value 0.1
--                <> long "adjoin-prob"
--                <> short 'a' )
--         <*> option
--                 auto
--                 ( metavar "TREE-NUM"
--                <> value 10
--                <> long "tree-num"
--                <> short 'n' ))
-- 
-- 
-- --------------------------------------------------
-- -- Stats options
-- --------------------------------------------------
-- 
-- 
-- statsOptions :: Parser Command
-- statsOptions = Stats
--     <$> buildDataParser
--     <*> (S.StatCfg
--           <$> option
--                   ( Just <$> auto )
--                   ( metavar "MAX-SIZE"
--                  <> value Nothing
--                  <> long "max-size"
--                  <> short 'm' )
--           <*> buildCfgParser
--           <*> strOption
--                 ( metavar "START-SYM"
--                <> long "start-sym"
--                <> short 's' )
--           <*> option
--             auto
--              ( metavar "PRINT-PARSED"
--                <> value 0
--                <> long "print-parsed"
--                <> short 'p' ) )
-- --          <*> switch
-- --                ( long "print-parsed-trees"
-- --               <> short 'p' ) )
-- 
-- 
-- --------------------------------------------------
-- -- Selection options
-- --------------------------------------------------
-- 
-- 
-- selectOptions :: Parser Command
-- selectOptions = fmap Select $ S.SelectCfg
--     <$> option
--             ( Just <$> auto )
--             ( metavar "MAX-SIZE"
--            <> value Nothing
--            <> long "max-size"
--            <> short 'm' )
--     <*> option
--             auto
--             ( metavar "SELECT-NUM"
--            <> value 100
--            <> long "select-num"
--            <> short 'n' )
--     <*> option
--             auto
--             ( metavar "MERGE-NUM"
--            <> value 1
--            <> long "merge-num"
--            <> short 'k' )
-- 
-- 
-- --------------------------------------------------
-- -- AStar options
-- --------------------------------------------------
-- 
-- 
-- astarOptions :: Parser Command
-- astarOptions = AStar
--     <$> buildDataParser
--     <*> strOption
--         ( long "start-sym"
--        <> short 's'
--        <> metavar "START-SYM"
--        <> help "Start parsing from symbol" )
--     <*> switch
--         ( long "show-trees"
--        <> help "Show parsed trees" )


--------------------------------------------------
-- Global options
--------------------------------------------------


opts :: Parser Command
opts = subparser
        ( command "map"
            (info (helper <*> (Map <$> mapCfgParser))
                (progDesc "Map MWEs on Składnica")
                )
--         <> command "parse"
--             (info (helper <*> parseOptions)
--                 (progDesc "Parse the input grammar file")
--                 )
--         <> command "trees"
--             (info (helper <*> (Trees <$> buildDataParser))
--                 (progDesc "Show elementary trees, no FSs")
--                 )
--         <> command "gen"
--             (info (helper <*> genOptions)
--                 (progDesc "Generate trees based on input grammar file")
--                 )
--         <> command "gen-rand"
--             (info (helper <*> genRandOptions)
--                 (progDesc "Generate and parse trees")
--                 )
--         <> command "stats"
--             (info (helper <*> statsOptions)
--                 (progDesc "Parse sentences from stdin")
--                 )
--         <> command "select"
--             (info (helper <*> selectOptions)
--                 (progDesc "Select sentences from stdin")
--                 )
--         <> command "lexicon"
--             (info (helper <*> lexicOptions)
--                 (progDesc "Parse and print the lexicon")
--                 )
--         <> command "print"
--             (info (helper <*> (Print <$> buildDataParser))
--                 (progDesc "Parse and print the lexicon")
--                 )
--         <> command "rules"
--             (info (helper <*> (Rules <$> buildDataParser))
--                 (progDesc "Print standard rules; experimental mode")
--                 )
--         <> command "weights"
--             (info (helper <*> (Weights <$> buildDataParser))
--                 (progDesc "Print weighted rules; experimental mode")
--                 )
--         <> command "astar"
--             (info (helper <*> astarOptions)
--                 (progDesc "Parse with A*; experimental mode")
--                 )
        )


-- | Run program depending on the cmdline arguments.
run :: Command -> IO ()
run cmd =
  case cmd of
    Map cfg -> E.mapMWEs cfg

--          Trees buildData ->
--             B.printTrees buildData
--          Parse grammarPath ->
--             P.printGrammar grammarPath
--          Print B.BuildData{..} ->
--             G.printTrees gramPath mayLexPath
--          Gen B.BuildData{..} sizeMax ->
--             G.generateFrom gramPath mayLexPath sizeMax
--          GenRand B.BuildData{..} cfg ->
--             G.genRandFrom cfg gramPath mayLexPath
--          Stats buildData cfg ->
--             S.statsOn cfg buildData
--          Select cfg ->
--             S.select cfg
--          Lexicon lexPath ->
--             L.printLexicon lexPath
--          Rules buildData ->
--             B.printRules buildData
--          Weights buildData ->
--             -- B.printWRules input lexicon
--             B.printWeiAuto buildData
--          AStar buildData begSym showTrees ->
--             S.parseWei buildData begSym showTrees


main :: IO ()
main =
    execParser optsExt >>= run
  where
    optsExt = info (helper <*> opts)
       ( fullDesc
      <> progDesc "Manipulating Składnica treebank"
      <> header "skladnica" )
