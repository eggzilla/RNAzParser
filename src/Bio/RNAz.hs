{-# LANGUAGE Arrows #-}

-- | Parse smiles strings
--   For more information on smiles strings consult: <http://>
module Bio.RNAz ( parseSmiles,
              module Bio.RNAz
            ) where

import Bio.RNAzData
--import Biobase.RNA
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (emptyDef)    
import Control.Monad

parseRNAzOutput :: GenParser Char st RNAzOutput
parseRNAzOutput = do
  skipMany1 space
  skipMany1 (char '#')
  skipMany1 space
  skipMany1 (string "RNAz" )
  skipMany1 space
  version <- (noneOf " ")
  skipMany1 space
  skipMany1 (char '#')
  skipMany1 space
  skipMany1 newline
  skipMany1 space
  skipMany1 newline
--              
  sequences <- many1 (parseRNAzIntField "Sequences:")
  columns <- many1 (parseRNAzIntField "Columns:")
  readingDirection <- many1 (parseRNAzStringField "Reading direction:")
  meanPairwiseIdentity <- many1 (parseRNAzDoubleField "Mean pairwise identity:")
  meanSingleSequenceMFE <- many1 (parseRNAzDoubleField "Mean single sequence MFE:")
  consensusMFE <- many1 (parseRNAzDoubleField "Consensus MFE:")
  energyContribution <- many1 (parseRNAzDoubleField "Energy contribution:")
  covarianceContribution <- many1 (parseRNAzDoubleField "Covariance contribution:")
  combinationsPair <- many1 (parseRNAzDoubleField "Combinations/Pair:")
  meanZScore <- many1 (parseRNAzDoubleField "Mean z-score:")
  structureConservationIndex <- many1 (parseRNAzDoubleField "Structure conservation index:")
  svmDecisionValue <- many1 (parseRNAzDoubleField "SVM decision value:")
  svmRNAClassProbability <- many1 (parseRNAzDoubleField "SVM RNA-class probability:")
  prediction <- many1 (parseRNAzStringField "Prediction:")
--
  skipMany1 space
  skipMany1 (char '#')
  skipMany1 space
  skipMany1 newline
  rnaZResults  <- many1 parseRNAzResult
  rnaZConsensus <- many1 parseRNAzConsensus         
  return $ RNAzOutput version sequences columns readingDirection meanPairwiseIdentity meanSingleSequenceMFE consensusMFE energyContribution combinationsPair meanZScore structureConservationIndex svmDecisionValue svmRNAClassProbability prediction rnaZResults rnaZConsensus


parseRNAzDoubleField :: String -> GenParser Char st Double
parseRNAzDoubleField fieldname = do
  skipMany1 space
  skipMany1 (string fieldname)
  skipMany1 space
  double <- many1 (noneOf " ")
  skipMany1 space
  skipMany1 newline
  return $ double
            

parseRNAzStringField :: String -> GenParser Char st String
parseRNAzStringField fieldname = do
  skipMany1 space
  skipMany1 (string fieldname)
  skipMany1 space
  string <- many1 (noneOf " ")
  skipMany1 space
  skipMany1 newline
  return $ string          

parseRNAzIntField :: String -> GenParser Char st Int
parseRNAzIntField fieldname = do
  skipMany1 space
  skipMany1 (string fieldname)
  skipMany1 space
  int <- many1 (noneOf " ")
  skipMany1 space
  skipMany1 newline
  return $ int
         
parseRNAzResult :: GenParser Char st RNAzResult
parseRNAzResult = do
  skipMany1 space
  header <- (noneOf newline)
  skipMany1 newline
  skipMany1 space
  resultSequence <- many1 (oneOf "ATUGCatugc")
  skipMany1 space                  
  skipMany1 newline
  skipMany1 space          
  dotBracket <- many1 (oneOf "().,")
  skipMany1 space
  char ('(')
  skipMany1 space
  mfe <- (noneOf " ")
  -- probably skipMany       
  skipMany1 space
  char (')')
  return $ RNAzResult header resultSequence dotBracket mfe
         
parseRNAzConsensus :: GenParser Char st RNAzConsensus
parseRNAzConsensus = do
  skipMany1 space
  string (">consensus")
  skipMany1 space       
  skipMany1 newline
  skipMany1 space
  consensusSequence <- many1 (oneOf "ATUGCatugc")
  skipMany1 space                  
  skipMany1 newline
  skipMany1 space          
  dotBracket <- many1 (oneOf "().,")
  skipMany1 space
  skipMany1 <- (noneOf eof)
  skipMany1 eof
       
  return $ RNAzConsensus consensusSequence dotBracket
