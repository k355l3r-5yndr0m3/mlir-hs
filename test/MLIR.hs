module Main (main) where
import Control.Exception
import MLIR.IR

main :: IO ()
main = do
  withMlirContext bracket $ \ context -> 
    
    return ()
