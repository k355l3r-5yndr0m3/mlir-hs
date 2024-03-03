{-# LANGUAGE ForeignFunctionInterface #-}
module MLIR.FFI.Finalizers where
import Data.Void
import Foreign.ForeignPtr

foreign import ccall unsafe "&mlirDialectRegistryDestroy"
  mlirDialectRegistryDestroy__finalizer :: FinalizerPtr Void

foreign import ccall unsafe "&mlirContextDestroy"
  mlirContextDestroy__finalizer :: FinalizerPtr Void
