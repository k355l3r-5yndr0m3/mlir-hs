module MLIR.Dialect.ControlFlow (
  dialect
, module Ops
) where
import MLIR.C.IR
import MLIR.Dialect.ControlFlow.Ops as Ops

foreign import ccall unsafe "mlirGetDialectHandle__cf__" 
  dialect :: DialectHandle
