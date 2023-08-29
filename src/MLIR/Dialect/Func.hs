{-# LANGUAGE ForeignFunctionInterface #-}
module MLIR.Dialect.Func (
  module Ops
, dialect
) where
import MLIR.C.IR

import MLIR.Dialect.Func.Ops as Ops

foreign import ccall unsafe "mlirGetDialectHandle__func__" dialect :: DialectHandle
