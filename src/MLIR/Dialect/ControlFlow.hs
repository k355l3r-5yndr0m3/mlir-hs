{-# LANGUAGE ForeignFunctionInterface #-}
module MLIR.Dialect.ControlFlow (
  module Ops 
, dialect
) where
import MLIR.C.IR
import MLIR.Dialect.ControlFlow.Ops as Ops

foreign import ccall unsafe "mlirGetDialectHandle__cf__" dialect :: DialectHandle
