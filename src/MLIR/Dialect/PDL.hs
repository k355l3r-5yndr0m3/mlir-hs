{-# LANGUAGE TemplateHaskell #-}
module MLIR.Dialect.PDL where
import MLIR.FFI.IR
import MLIR.TH

foreign import ccall unsafe "mlirGetDialectHandle__pdl__"
  dialectHandle :: MlirDialectHandle
$(generateMlirDialect "mlir/Dialect/PDL/IR/PDLOps.td")
