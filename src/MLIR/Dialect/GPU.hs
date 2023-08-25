module MLIR.Dialect.GPU (
  dialect
, module Ops
) where
import MLIR.C.IR
import MLIR.Dialect.GPU.Ops as Ops

foreign import ccall unsafe "mlirGetDialectHandle__gpu__"
  dialect :: DialectHandle
