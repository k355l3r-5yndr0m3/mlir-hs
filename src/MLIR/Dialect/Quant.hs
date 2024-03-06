{-# LANGUAGE TemplateHaskell #-}
module MLIR.Dialect.Quant where
import MLIR.FFI.IR
import MLIR.TH

foreign import ccall unsafe "mlirGetDialectHandle__quant__"
  dialectHandle :: MlirDialectHandle
$(generateMlirDialect "mlir/Dialect/Quant/QuantOps.td")
