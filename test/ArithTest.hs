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
      let body = mlirModuleGetBody _module
      (_, _0) <- Arith._constant context body ul (mlirIntegerAttrGet i32 0) i32
      (_, _1) <- Arith._constant context body ul (mlirIntegerAttrGet i32 1) i32
      _       <- Arith._addi context body ul _0 _1 i32
      -- _ <- Func._func
      --       context body ul
      --       (mlirStringAttrGet context "main")
      --       (mlirTypeAttrGet $ mlirFunctionTypeGet context [] [])
      --       Nothing
      --       Nothing
      --       Nothing
      --       (\ main_func_region -> do 
      --           main_func_bb0 <- mlirRegionAddBlock main_func_region [] 
      --           _ <- Func._call context main_func_bb0 ul [] (mlirFlatSymbolRefAttrGet context "main") []
      --           _ <- Func._return context main_func_bb0 ul []
      --           return ()
      --       )
      mlirModuleDump _module
      print =<< mlirOperationVerify (mlirModuleGetOperation _module)
  return ()


