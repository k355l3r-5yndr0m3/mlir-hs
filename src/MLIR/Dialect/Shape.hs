{-# LANGUAGE TemplateHaskell #-}
module MLIR.Dialect.Shape where
import MLIR.FFI.IR
import MLIR.TH

foreign import ccall unsafe "mlirGetDialectHandle__shape__"
  dialectHandle :: MlirDialectHandle
$(generateMlirDialect "mlir/Dialect/Shape/IR/ShapeOps.td")
