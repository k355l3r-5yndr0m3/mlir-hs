{-# LANGUAGE ForeignFunctionInterface #-}
module MLIR.Dialect.Math (
  dialect 
, module Ops
) where
import MLIR.C.IR

import MLIR.Dialect.Math.Ops as Ops

foreign import ccall unsafe "mlirGetDialectHandle__math__" dialect :: DialectHandle
