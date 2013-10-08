-- | This module contains a hierarchical data structure for smiles strings
--   For more information on smiles strings consult: <http://meme.nbcr.net/meme/>

module Bio.RNAzData where

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
    rnazResults :: [RNAzResult]   
  }
  deriving (Show, Eq)


data RNAzResult = RNAzResult
  { header :: String,
    sequence :: String,
    mfe :: Double,
  }
  deriving (Show, Eq)
