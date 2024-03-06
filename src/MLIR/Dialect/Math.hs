{-# LANGUAGE TemplateHaskell #-}
module MLIR.Dialect.Math where
import MLIR.FFI.IR
import MLIR.TH

foreign import ccall unsafe "mlirGetDialectHandle__math__"
  dialectHandle :: MlirDialectHandle
$(generateMlirDialect "mlir/Dialect/Math/IR/MathOps.td")
