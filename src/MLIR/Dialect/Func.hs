{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module MLIR.Dialect.Func where
import MLIR.FFI.IR
import MLIR.TH

foreign import ccall unsafe "mlirGetDialectHandle__func__"
  dialectHandle__func__ :: MlirDialectHandle

$(generateMlirDialect "mlir/Dialect/Func/IR/FuncOps.td")
