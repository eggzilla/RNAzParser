{-# LANGUAGE Arrows #-}

-- | Parse smiles strings
--   For more information on smiles strings consult: <http://>
module Bio.RNAzParser (
                       parseRNAzOutput,
                       module Bio.RNAzData
                      ) where

import Bio.RNAzData
--import Biobase.RNA
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (emptyDef)    
import Control.Monad

readDouble :: String -> Double
readDouble = read              

readInt :: String -> Int
readInt = read

parseRNAzOutput :: GenParser Char st RNAzOutput
parseRNAzOutput = do
  skipMany1 space
  skipMany1 (char '#')
  skipMany1 space
  skipMany1 (string "RNAz" )
  skipMany1 space
  version <- many1 (noneOf " ")
  skipMany1 space
  skipMany1 (char '#')
  skipMany1 space
  skipMany1 newline
  skipMany1 space
  skipMany1 newline
  sequences <- parseRNAzIntField "Sequences:"
  columns <- parseRNAzIntField "Columns:"
  readingDirection <- parseRNAzStringField "Reading direction:"
  meanPairwiseIdentity <- parseRNAzDoubleField "Mean pairwise identity:"
  meanSingleSequenceMFE <- parseRNAzDoubleField "Mean single sequence MFE:"
  consensusMFE <- parseRNAzDoubleField "Consensus MFE:"
  energyContribution <- parseRNAzDoubleField "Energy contribution:"
  covarianceContribution <- parseRNAzDoubleField "Covariance contribution:"
  combinationsPair <- parseRNAzDoubleField "Combinations/Pair:"
  meanZScore <- parseRNAzDoubleField "Mean z-score:"
  structureConservationIndex <- parseRNAzDoubleField "Structure conservation index:"
  svmDecisionValue <-  parseRNAzDoubleField "SVM decision value:"
  svmRNAClassProbability <- parseRNAzDoubleField "SVM RNA-class probability:"
  prediction <- parseRNAzStringField "Prediction:"
  skipMany1 space
  skipMany1 (char '#')
  rnaZResults  <- many1 parseRNAzResult
  rnaZConsensus <- parseRNAzConsensus         
  return $ RNAzOutput version sequences columns readingDirection meanPairwiseIdentity meanSingleSequenceMFE consensusMFE energyContribution covarianceContribution combinationsPair meanZScore structureConservationIndex svmDecisionValue svmRNAClassProbability prediction rnaZResults rnaZConsensus

parseRNAzDoubleField :: String -> GenParser Char st Double
parseRNAzDoubleField fieldname = do
  skipMany1 space
  skipMany1 (string fieldname)
  skipMany1 space
  double <- (many1 (noneOf " ")) 
  skipMany1 space
  skipMany1 newline
  return $ (readDouble double)
            

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
  return $ (readInt int)
         
parseRNAzResult :: GenParser Char st RNAzResult
parseRNAzResult = do
  skipMany1 space
  skipMany1 newline
  skipMany1 space
  header <- many1 (noneOf "\n")
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
  mfe <- many1 (noneOf " ")      
  skipMany1 space
  char (')')
  return $ RNAzResult header resultSequence dotBracket (readDouble mfe)
         
parseRNAzConsensus :: GenParser Char st RNAzConsensus
parseRNAzConsensus = do
  skipMany1 space
  skipMany1 newline
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
  skipMany1 anyChar
  eof   
  return $ RNAzConsensus consensusSequence dotBracket

-- | Parser for RNAz output files
getRNAzOutput filePath = let
        fp = filePath
        doParseLine' = parse parseRNAzOutput "parseRNAzOutput"
        doParseLine l = case (doParseLine' l) of
            Right x -> x
            Left _  -> error "Failed to parse line"
    in do
        fileContent <- liftM lines $ readFile fp
        return $ map doParseLine' fileContent
