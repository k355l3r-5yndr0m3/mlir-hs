{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module MLIR.Dialect.Func
( module MLIR.Dialect.Func
, module Extra
)where
import MLIR.Dialect.Func.Extra as Extra
import MLIR.FFI.IR
import MLIR.TH

foreign import ccall unsafe "mlirGetDialectHandle__func__"
  dialectHandle :: MlirDialectHandle

$(generateMlirDialect "mlir/Dialect/Func/IR/FuncOps.td")

