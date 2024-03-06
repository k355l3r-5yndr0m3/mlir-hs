{-# LANGUAGE TemplateHaskell #-}
module MLIR.Dialect.SparseTensor where
import MLIR.FFI.IR
import MLIR.TH

foreign import ccall unsafe "mlirGetDialectHandle__sparse_tensor__"
  dialectHandle :: MlirDialectHandle
$(generateMlirDialect "mlir/Dialect/SparseTensor/IR/SparseTensorOps.td")
