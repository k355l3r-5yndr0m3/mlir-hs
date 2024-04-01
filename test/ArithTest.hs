module Main (main) where

import MLIR.BuiltinAttributes
import MLIR.BuiltinTypes
import MLIR.IR

-- import qualified MLIR.Dialect.Func as Func
import qualified MLIR.Dialect.Arith as Arith


main :: IO ()
main = do
  withMlirContext $ \ context -> 
    do
    let ul  = mlirLocationUnknownGet context
        i32 = mlirIntegerTypeGet context 32
    _ <- mlirDialectHandleLoadDialect Arith.dialectHandle context
    -- _ <- mlirDialectHandleLoadDialect Func.dialectHandle context
    withMlirModule (mlirLocationUnknownGet context) $ \ _module -> 
      do
      print =<< mlirOperationVerify (mlirModuleGetOperation _module)
  return ()


