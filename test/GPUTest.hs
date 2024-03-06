module Main (main) where

import MLIR.BuiltinAttributes
import MLIR.BuiltinTypes
import MLIR.IR

import MLIR.Dialect.GPU as GPU

main :: IO ()
main = do
  withMlirContext $ \ context -> 
    do
    let ul  = mlirLocationUnknownGet context
        i32 = mlirIntegerTypeGet context 32
    _ <- mlirDialectHandleLoadDialect GPU.dialectHandle context
    withMlirModule (mlirLocationUnknownGet context) $ \ _module -> 
      do
      let body = mlirModuleGetBody _module
      
      mlirModuleDump _module
      print =<< mlirOperationVerify (mlirModuleGetOperation _module)
  return ()


