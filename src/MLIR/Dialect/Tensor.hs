{-# LANGUAGE TemplateHaskell #-}
module MLIR.Dialect.Tensor where
import MLIR.FFI.IR
import MLIR.TH

foreign import ccall unsafe "mlirGetDialectHandle__tensor__"
  dialectHandle :: MlirDialectHandle
$(generateMlirDialect "mlir/Dialect/Tensor/IR/TensorOps.td")
