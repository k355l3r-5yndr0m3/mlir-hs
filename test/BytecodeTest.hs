module Main (main) where
import Control.Exception
import MLIR.IR
import System.Directory


main :: IO ()
main = do
  withMlirContext bracket $ \ context -> 
    do
    let ul = mlirLocationUnknownGet context
    withMlirModule (mlirLocationUnknownGet context) bracket $ \ _module -> 
      do
      bytecode <- mlirModuleEmitBytecode _module
      writeBytecodeToFile bytecode outputFile
  removeFile outputFile
  return ()
  where outputFile = "mlir-bytecode.bin"


