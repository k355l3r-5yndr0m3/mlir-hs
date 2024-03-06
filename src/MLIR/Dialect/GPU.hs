{-# LANGUAGE TemplateHaskell #-}
module MLIR.Dialect.GPU where
import MLIR.FFI.IR
import MLIR.TH

foreign import ccall unsafe "mlirGetDialectHandle__gpu__"
  dialectHandle :: MlirDialectHandle
$(generateMlirDialect "mlir/Dialect/GPU/IR/GPUOps.td")
