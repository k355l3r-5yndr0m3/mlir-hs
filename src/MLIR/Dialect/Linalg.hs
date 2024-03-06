{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module MLIR.Dialect.Linalg where
import MLIR.FFI.IR
import MLIR.TH

foreign import ccall unsafe "mlirGetDialectHandle__linalg__"
  dialectHandle :: MlirDialectHandle

$(generateMlirDialect "mlir/Dialect/Linalg/IR/LinalgOps.td")
