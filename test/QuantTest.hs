module Main (main) where

import MLIR.BuiltinAttributes
import MLIR.BuiltinTypes
import MLIR.IR

import qualified MLIR.Dialect.Quant as Quant

main :: IO ()
main = do
  withMlirContext $ \ context -> 
    do
    let ul  = mlirLocationUnknownGet context
        i32 = mlirIntegerTypeGet context 32
    _ <- mlirDialectHandleLoadDialect Quant.dialectHandle context
    withMlirModule (mlirLocationUnknownGet context) $ \ _module -> 
      do
      let body = mlirModuleGetBody _module
      
      mlirModuleDump _module
      print =<< mlirOperationVerify (mlirModuleGetOperation _module)
  return ()


