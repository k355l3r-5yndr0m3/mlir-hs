{-# LANGUAGE ForeignFunctionInterface #-}
module MLIR.Dialect.Async (
  module Ops 
, dialect
) where
import MLIR.C.IR
import MLIR.Dialect.Async.Ops as Ops

foreign import ccall unsafe "mlirGetDialectHandle__async__" dialect :: DialectHandle
