-- | Parser test script
--   read from file and directly print parsing output

module Main where
    
import System.Environment (getArgs)
import System.Process 
import Text.ParserCombinators.Parsec
import System.IO
import System.Environment
import Data.List
import Bio.RNAzParser
import System.Directory
import System.Cmd
import Control.Monad    
import Data.Either
import Data.Either.Unwrap
    
main = do
  args <- getArgs
  let input_file = (head args)
  let output_file = (last args)
  input_present <- doesFileExist input_file
  output_present <- doesFileExist output_file                   

  --show input_present
  let input_present_string = show input_present
  let output_present_string = show output_present                          

  -- read RNAz outputfile
  input_file_content <- readFile input_file                       
  print (fromLeft (getRNAzOutput input_file_content))
  
