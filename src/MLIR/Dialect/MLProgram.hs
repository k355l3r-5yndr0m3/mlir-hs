{-# LANGUAGE TemplateHaskell #-}
module MLIR.Dialect.MLProgram where
import MLIR.FFI.IR
import MLIR.TH

foreign import ccall unsafe "mlirGetDialectHandle__ml_program__"
  dialectHandle :: MlirDialectHandle
$(generateMlirDialect "mlir/Dialect/MLProgram/IR/MLProgramOps.td")
