{-# LANGUAGE ForeignFunctionInterface #-}
module MLIR.Dialect.Arith (
  dialect
, module Ops
, module Attributes
) where
import MLIR.C.IR

import MLIR.Dialect.Arith.Ops as Ops
import MLIR.Dialect.Arith.Attributes as Attributes

foreign import ccall unsafe "mlirGetDialectHandle__arith__" dialect :: DialectHandle
