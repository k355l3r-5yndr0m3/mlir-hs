{-# LANGUAGE TemplateHaskell #-}
module MLIR.Dialect.ControlFlow where
import MLIR.FFI.IR
import MLIR.TH

foreign import ccall unsafe "mlirGetDialectHandle__cf__"
  dialectHandle :: MlirDialectHandle
$(generateMlirDialect "mlir/Dialect/ControlFlow/IR/ControlFlowOps.td")
