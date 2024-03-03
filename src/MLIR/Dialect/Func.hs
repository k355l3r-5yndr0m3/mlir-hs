{-# LANGUAGE ForeignFunctionInterface #-}
module MLIR.Dialect.Func where
import MLIR.FFI.IR
import MLIR.IR

foreign import ccall unsafe "mlirGetDialectHandle__func__"
  dialectHandle__func__ :: DialectHandle

