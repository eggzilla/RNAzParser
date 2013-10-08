-- | This module contains a hierarchical data structure for smiles strings
--   For more information on smiles strings consult: <http://meme.nbcr.net/meme/>

module Bio.RNAzData where
--import Biobase.RNA
    
-- | 
data RNAzOutput = RNAzOutput
  { version :: String,
    sequenceNumber :: Int,
    columnNumber :: Int,
    readingDirection :: String,
    meanPairwiseIdentity :: Double,
    meanSingleSequenceMinimumFreeEnergy :: Double,
    consensusMinimumFreeEnergy :: Double,
    energyContribution :: Double,
    covarianceContribution :: Double,
    combinationsPair :: Double,
    meanZScore :: Double,
    structureConservationIndex :: Double,
    svmDecisionValue :: Double,
    svmRNAClassProbability :: Double,
    prediction :: String,
    rnazResults :: [RNAzResult],
    rnazConsensus :: RNAzConsensus
  }
  deriving (Show, Eq)

data RNAzResult = RNAzResult
  { header :: String,
    resultSequence :: String,
    dotBracket :: String,         
    minimumFreeEnergy :: Double
  }
  deriving (Show, Eq)

data RNAzConsensus = RNAzConsensus
  { consensusSequence :: String,
    consensusDotBracket :: String         
  }
  deriving (Show, Eq)           
