{-# LANGUAGE ForeignFunctionInterface #-}
module MLIR.Dialect.Arith (
  module Ops 
, dialect
) where
import MLIR.C.IR
import MLIR.Dialect.Arith.Ops as Ops

foreign import ccall unsafe "mlirGetDialectHandle__arith__" dialect :: DialectHandle
