{-# LANGUAGE TemplateHaskell #-}
module MLIR.Dialect.Async where
import MLIR.FFI.IR
import MLIR.TH

foreign import ccall unsafe "mlirGetDialectHandle__async__"
  dialectHandle :: MlirDialectHandle
$(generateMlirDialect "mlir/Dialect/Async/IR/AsyncOps.td")
