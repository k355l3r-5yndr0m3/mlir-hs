{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module MLIR.Dialect.LLVM
( module MLIR.Dialect.LLVM
, module Extra
) where
import MLIR.FFI.IR
import MLIR.TH
import MLIR.Dialect.LLVM.Extra as Extra

foreign import ccall unsafe "mlirGetDialectHandle__llvm__"
  dialectHandle :: MlirDialectHandle
$(generateMlirDialect "mlir/Dialect/LLVMIR/LLVMOps.td")
-- TODO Import attributes
