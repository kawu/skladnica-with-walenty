{-# LANGUAGE RecordWildCards   #-}


-- | A module for pruning arguments uninteresting from our point of view, so
-- that only obligatory arguments are left and it is clear how to handle them.


module NLP.Skladnica.Walenty.Prune
( pruneVerb
) where


-- import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Control.Monad             (guard)
import           Data.Maybe                (catMaybes)

import qualified NLP.Walenty.Types         as W


-- | Prune the verbal entry.
pruneVerb :: W.Verb -> Maybe W.Verb
pruneVerb verb = case pruneFrame (W.frame verb) of
  [] -> Nothing
  xs -> Just $ verb {W.frame = xs}


-- | Prune the frame of the entry.
pruneFrame :: W.Frame -> W.Frame
pruneFrame = catMaybes . map pruneArg


-- | Prune the argument of the entry.
pruneArg :: W.Argument -> Maybe W.Argument
pruneArg arg = case pruneAlt (W.phraseAlt arg) of
  [] -> Nothing
  xs -> Just $ arg {W.phraseAlt = xs}


-- | Prune the alternative realizations of an argument.
pruneAlt :: [W.Phrase] -> [W.Phrase]
pruneAlt = catMaybes . map prunePhrase


-- | Prune the phrase.
prunePhrase :: W.Phrase -> Maybe W.Phrase
prunePhrase (W.Standard s) = W.Standard <$> pruneStd s
prunePhrase (W.Special s) = W.Special <$> pruneSpec s


-- | Prune the special phrase.
pruneSpec :: W.SpecPhrase -> Maybe W.SpecPhrase
pruneSpec p@W.ComPrepNP{} = Just p
pruneSpec p@W.XP{} = do
  val <- prunePhrase =<< (W.xpVal p)
  return $ p {W.xpVal = Just val}
pruneSpec _ = Nothing
-- pruneSpec p@W.Fixed{} = Just p


pruneStd :: W.StdPhrase -> Maybe W.StdPhrase
pruneStd p@W.ComparP{..} = do
  let alt = pruneAlt comparFrame
  guard $ (not . null) (W.lexicalHead p) || (not . null) alt
  return p {W.comparFrame = alt}
pruneStd p = do
  let deps = pruneDeps (W.dependents p)
  guard $ (not . null) (W.lexicalHead p) || isLexicalDeps deps
  return p {W.dependents = deps}


-- | Prune dependencies.
pruneDeps :: W.Attribute -> W.Attribute
pruneDeps attr = case attr of
  W.NAtr -> W.NAtr
  W.Atr f -> W.Atr (pruneFrame f)
  W.Atr1 f -> W.Atr1 (pruneFrame f)
  W.RAtr f -> case pruneFrame f of
    [] -> W.Atr []
    xs -> W.RAtr xs
  W.RAtr1 f -> case pruneFrame f of
    [] -> W.Atr []
    xs -> W.RAtr1 xs


-- | Are there any lexicalized dependencies?
isLexicalDeps :: W.Attribute -> Bool
isLexicalDeps attr = case attr of
  W.NAtr -> False
  W.Atr f -> lex f
  W.Atr1 f -> lex f
  W.RAtr f -> lex f
  W.RAtr1 f -> lex f
  where
    lex = not . null


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------


-- maybeT :: Monad m => Maybe a -> MaybeT m a
-- maybeT = MaybeT . return
