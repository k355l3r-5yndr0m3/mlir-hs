{-# LANGUAGE ForeignFunctionInterface #-}
module MLIR.Dialect.Func (
  module Ops
, dialect
) where

import MLIR.Dialect.Func.Ops as Ops

import MLIR.C.IR

foreign import ccall unsafe "mlirGetDialectHandle__func__" dialect :: DialectHandle

