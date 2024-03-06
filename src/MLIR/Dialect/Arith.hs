{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module MLIR.Dialect.Arith where
import MLIR.FFI.IR
import MLIR.TH

foreign import ccall unsafe "mlirGetDialectHandle__arith__"
  dialectHandle :: MlirDialectHandle

$(generateMlirDialect "mlir/Dialect/Arith/IR/ArithOps.td")
