module MLIR.Dialect.Linalg (
  dialect
, module Ops
) where
import MLIR.C.IR
import MLIR.Dialect.Linalg.Ops as Ops

foreign import ccall unsafe "mlirGetDialectHandle__linalg__"
  dialect :: DialectHandle
