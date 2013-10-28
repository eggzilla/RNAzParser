{-# LANGUAGE Arrows #-}

-- | Parse smiles strings
--   For more information on smiles strings consult: <http://>
module Bio.RNAzParser (
                       getRNAzOutput,
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
  char '\n'
  many1 (oneOf "# ")
  string "RNAz"
  space
  version <- many1 (noneOf " ")
  space
  space
  many1 (char '#')
  newline  
  space
  sequences <- parseRNAzIntField "Sequences:"
  columns <- parseRNAzIntField "Columns:"
  readingDirection <- parseRNAzStringField "Reading direction:"
  meanPairwiseIdentity <- parseRNAzDoubleField "Mean pairwise identity:"
  shannonEntropy <- parseRNAzDoubleField "Shannon entropy:" 
  gcContent <- parseRNAzDoubleField "G+C content:"
  meanSingleSequenceMFE <- parseRNAzDoubleField "Mean single sequence MFE:"
  consensusMFE <- parseRNAzDoubleField "Consensus MFE:"
  energyContribution <- parseRNAzDoubleField "Energy contribution:"
  covarianceContribution <- parseRNAzDoubleField "Covariance contribution:"
  combinationsPair <- parseRNAzDoubleField "Combinations/Pair:"
  meanZScore <- parseRNAzDoubleField "Mean z-score:"
  structureConservationIndex <- parseRNAzDoubleField "Structure conservation index:"
  backgroundModel <- parseRNAzStringField "Background model:"
  decisionModel <- parseRNAzStringField "Decision model:"
  svmDecisionValue <-  parseRNAzDoubleField "SVM decision value:"
  svmRNAClassProbability <- parseRNAzDoubleField "SVM RNA-class probability:"
  prediction <- parseRNAzStringField "Prediction:"
  space
  many1 (char '#')
  newline
  rnaZResults  <- many1 (try parseRNAzResult)
  rnaZConsensus <- parseRNAzConsensus         
  return $ RNAzOutput version sequences columns readingDirection meanPairwiseIdentity shannonEntropy gcContent meanSingleSequenceMFE consensusMFE energyContribution covarianceContribution combinationsPair meanZScore structureConservationIndex  backgroundModel decisionModel svmDecisionValue svmRNAClassProbability prediction rnaZResults rnaZConsensus

parseRNAzDoubleField :: String -> GenParser Char st Double
parseRNAzDoubleField fieldname = do
  optional space
  string fieldname
  many1 space
  double <- (many1 (noneOf " ")) 
  space
  return $ (readDouble double)
            
parseRNAzStringField :: String -> GenParser Char st String
parseRNAzStringField fieldname = do
  optional space
  string fieldname
  space
  string <- many1 (noneOf "\n")
  space
  return $ string          

parseRNAzIntField :: String -> GenParser Char st Int
parseRNAzIntField fieldname = do
  optional space
  string fieldname
  space
  int <- many1 (noneOf " ")
  space
  return $ (readInt int)
         
parseRNAzResult :: GenParser Char st RNAzResult
parseRNAzResult = do
  space
  header <- many1 (noneOf "\n")
  many1 space
  resultSequence <- many1 (oneOf "ATUGCatugc")         
  newline        
  dotBracket <- many1 (oneOf "().,")
  space
  char ('(')
  space
  mfe <- many1 (noneOf ",")
  char ','
  space
  string ("z-score")
  space
  char '='
  space
  zscore <- many1 (noneOf ",")
  char ','
  space
  char 'R'
  char (')')
  return $ RNAzResult header resultSequence dotBracket (readDouble mfe) (readDouble zscore)
         
parseRNAzConsensus :: GenParser Char st RNAzConsensus
parseRNAzConsensus = do
  space
  string (">consensus")
  many1 space
  consensusSequence <- many1 (oneOf "ATUGCatugc")                 
  newline          
  dotBracket <- many1 (oneOf "().,")
  space
  char '('
  many1 (noneOf " ")
  space
  char '='
  space
  many1 (noneOf " ")
  space
  char '+'
  many1 space
  many1 (noneOf ")")
  char ')'
  space
  newline
  eof   
  return $ RNAzConsensus consensusSequence dotBracket


-- | parse RNAzOutput from input string
getRNAzOutput input = parse parseRNAzOutput "parseRNAzOutput" input

-- | parse from input filePath                      
parseRNAz :: String -> IO (Either ParseError RNAzOutput)                  
parseRNAz filePath = parseFromFile parseRNAzOutput filePath
