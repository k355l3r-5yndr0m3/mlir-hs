module MLIR.Dialect.Async (
  dialect
, module Ops
) where
import MLIR.C.IR
import MLIR.Dialect.Async.Ops as Ops

foreign import ccall unsafe "mlirGetDialectHandle__async__" 
  dialect :: DialectHandle
