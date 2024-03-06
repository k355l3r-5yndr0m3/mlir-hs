module Main (main) where
import MLIR.BuiltinAttributes
import MLIR.BuiltinTypes
import MLIR.IR

import qualified MLIR.Dialect.LLVM as LLVM

main :: IO ()
main = do
  withMlirContext $ \ context -> 
    do
    let ul = mlirLocationUnknownGet context
    _ <- mlirDialectHandleLoadDialect LLVM.dialectHandle context
    withMlirModule (mlirLocationUnknownGet context) $ \ _module -> 
      do
      let body = mlirModuleGetBody _module
      let _i32 = mlirIntegerTypeGet context 32
      (_, _0) <- LLVM._mlir_constant context body ul (mlirIntegerAttrGet _i32 0) _i32
      (_, _1) <- LLVM._mlir_constant context body ul (mlirIntegerAttrGet _i32 5) _i32
      (_, _2) <- LLVM._add context body ul _0 _1 _i32


      mlirModuleDump _module
      print =<< mlirOperationVerify (mlirModuleGetOperation _module)
  return ()


