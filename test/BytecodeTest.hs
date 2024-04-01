module Main (main) where
import MLIR.BuiltinAttributes
import MLIR.BuiltinTypes
import MLIR.IR


main :: IO ()
main = do
  withMlirContext $ \ context -> 
    do
    let ul = mlirLocationUnknownGet context
    withMlirModule (mlirLocationUnknownGet context) $ \ _module -> 
      do
      bytecode <- mlirModuleEmitBytecode _module
      writeBytecodeToFile bytecode outputFile
  return ()
  where outputFile = "mlir-bytecode.bin"


