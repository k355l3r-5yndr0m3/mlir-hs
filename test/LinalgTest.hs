module Main (main) where
import Control.Exception

import MLIR.BuiltinAttributes
import MLIR.BuiltinTypes
import MLIR.IR

import qualified MLIR.Dialect.Linalg as Linalg


main :: IO ()
main = do
  withMlirContext bracket $ \ context -> 
    do
    let ul  = mlirLocationUnknownGet context
        i32 = mlirIntegerTypeGet context 32
    _ <- mlirDialectHandleLoadDialect Linalg.dialectHandle context
    withMlirModule (mlirLocationUnknownGet context) bracket $ \ _module -> 
      do
      let body = mlirModuleGetBody _module
      mlirModuleDump _module
      print =<< mlirOperationVerify (mlirModuleGetOperation _module)
  return ()


