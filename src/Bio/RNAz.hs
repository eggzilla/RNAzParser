{-# LANGUAGE Arrows #-}

-- | Parse smiles strings
--   For more information on smiles strings consult: <http://>
module Chem.Smiles ( parseSmiles,
                     module Chem.SmilesData
                   ) where

import Chem.SmilesData
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (emptyDef)    
import Control.Monad

parseSmilesString :: GenParser Char st SmilesString
parseSmilesString = do
    identifier <-  many1 (noneOf " ")
    many1 space
    smilesString <-  many1 (noneOf " ")
    return $ SmilesString


