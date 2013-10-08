-- | This module contains a hierarchical data structure for smiles strings
--   For more information on smiles strings consult: <http://meme.nbcr.net/meme/>

module Chem.SmilesData where

-- | 
data SmilesString = SmilesString
  { identifier :: String,
    smilesString ::String }
  deriving (Show, Eq)
