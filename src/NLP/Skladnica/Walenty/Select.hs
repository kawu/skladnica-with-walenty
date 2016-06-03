-- | Provisional grammar selection module.


module NLP.Skladnica.Walenty.Select
( select
) where


import           Data.Text.Lazy                (Text)
import qualified Data.Set as S

import qualified NLP.Partage.Tree.Other       as O

import qualified NLP.Skladnica.Walenty.Grammar as G


-- | Select grammar ETs which have all their terminals
-- in the given set of terminals.
select
  :: S.Set Text -- ^ Set of terminals to which restrict the grammar
  -> S.Set G.ET -- ^ The grammar itself
  -> S.Set G.ET -- ^ Restricted grammar
select sentSet gram = S.fromList
  [ tree | tree <- S.toList gram
  , terminalSet tree `S.isSubsetOf` sentSet ]  
  where terminalSet = S.fromList . O.project
