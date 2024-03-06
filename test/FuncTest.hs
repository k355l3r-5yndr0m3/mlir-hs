module Main (main) where

import MLIR.BuiltinAttributes
import MLIR.BuiltinTypes
import MLIR.IR

import qualified MLIR.Dialect.Func as Func


main :: IO ()
main = do
  withMlirContext $ \ context -> 
    do
    let ul = mlirLocationUnknownGet context
    _ <- mlirDialectHandleLoadDialect Func.dialectHandle context
    withMlirModule (mlirLocationUnknownGet context) $ \ _module -> 
      do
      let body = mlirModuleGetBody _module
      _ <- Func._func
            context body ul
            (mlirStringAttrGet context "main")
            (mlirTypeAttrGet $ mlirFunctionTypeGet context [] [])
            Nothing
            Nothing
            Nothing
            (\ main_func_region -> do 
                main_func_bb0 <- mlirRegionAddBlock main_func_region [] 
                _ <- Func._call context main_func_bb0 ul [] (mlirFlatSymbolRefAttrGet context "main") []
                _ <- Func._return context main_func_bb0 ul []
                return ()
            )
      mlirModuleDump _module
      print =<< mlirOperationVerify (mlirModuleGetOperation _module)
  return ()


