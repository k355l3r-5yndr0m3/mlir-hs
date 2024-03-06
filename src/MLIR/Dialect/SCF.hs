{-# LANGUAGE TemplateHaskell #-}
module MLIR.Dialect.SCF where
import MLIR.FFI.IR
import MLIR.TH

foreign import ccall unsafe "mlirGetDialectHandle__scf__"
  dialectHandle :: MlirDialectHandle
$(generateMlirDialect "mlir/Dialect/SCF/IR/SCFOps.td")
