{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module MLIR.Dialect.LLVM where
import MLIR.FFI.IR
import MLIR.TH
import Data.Coerce

foreign import ccall unsafe "mlirGetDialectHandle__llvm__"
  dialectHandle :: MlirDialectHandle
$(generateMlirDialect "mlir/Dialect/LLVMIR/LLVMOps.td")
-- TODO Import attributes
