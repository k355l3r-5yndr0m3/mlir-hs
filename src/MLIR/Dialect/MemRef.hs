{-# LANGUAGE TemplateHaskell #-}
module MLIR.Dialect.MemRef where
import MLIR.FFI.IR
import MLIR.TH

foreign import ccall unsafe "mlirGetDialectHandle__memref__"
  dialectHandle :: MlirDialectHandle
$(generateMlirDialect "mlir/Dialect/MemRef/IR/MemRefOps.td")
