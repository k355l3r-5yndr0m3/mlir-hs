{-# LANGUAGE TemplateHaskell #-}
module MLIR.Dialect.Vector where
import MLIR.FFI.IR
import MLIR.TH

foreign import ccall unsafe "mlirGetDialectHandle__vector__"
  dialectHandle :: MlirDialectHandle
$(generateMlirDialect "mlir/Dialect/Vector/IR/VectorOps.td")
