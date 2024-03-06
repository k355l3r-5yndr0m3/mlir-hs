{-# LANGUAGE TemplateHaskell #-}
module MLIR.Dialect.Transform where
import MLIR.FFI.IR
import MLIR.TH

foreign import ccall unsafe "mlirGetDialectHandle__transform__"
  dialectHandle :: MlirDialectHandle
$(generateMlirDialect "mlir/Dialect/Transform/IR/TransformOps.td")
