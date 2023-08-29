{-# LANGUAGE ForeignFunctionInterface #-}
module MLIR.C.BuiltinTypes where
import MLIR.C.IR

import Foreign
import Foreign.C

foreign import ccall unsafe "mlirIntegerTypeGet"
  integerTypeGet :: Context -> CUInt -> IO Type
foreign import ccall unsafe "mlirIntegerTypeSignedGet"
  integerTypeSignedGet :: Context -> CUInt -> IO Type
foreign import ccall unsafe "mlirIntegerTypeUnsignedGet"
  integerTypeUnsignedGet :: Context -> CUInt -> IO Type

foreign import ccall unsafe "mlirIndexTypeGet"
  indexTypeGet :: Context -> IO Type

foreign import ccall unsafe "mlirFloat8E5M2TypeGet"
  float8E5M2TypeGet :: Context -> IO Type
foreign import ccall unsafe "mlirFloat8E4M3FNTypeGet"
  float8E4M3FNTypeGet :: Context -> IO Type
foreign import ccall unsafe "mlirFloat8E5M2FNUZTypeGet"
  float8E5M2FNUZTypeGet :: Context -> IO Type
foreign import ccall unsafe "mlirFloat8E4M3FNUZTypeGet"
  float8E4M3FNUZTypeGet :: Context -> IO Type
foreign import ccall unsafe "mlirFloat8E4M3B11FNUZTypeGet"
  float8E4M3B11FNUZTypeGet :: Context -> IO Type
foreign import ccall unsafe "mlirBF16TypeGet"
  bf16TypeGet :: Context -> IO Type
foreign import ccall unsafe "mlirF16TypeGet"
  f16TypeGet :: Context -> IO Type
foreign import ccall unsafe "mlirF32TypeGet"
  f32TypeGet :: Context -> IO Type
foreign import ccall unsafe "mlirF64TypeGet"
  f64TypeGet :: Context -> IO Type
foreign import ccall unsafe "mlirTF32TypeGet"
  tf32TypeGet :: Context -> IO Type

foreign import ccall unsafe "mlirNoneTypeGet"
  noneTypeGet :: Context -> IO Type
foreign import ccall unsafe "mlirComplexTypeGet"
  complexTypeGet :: Type -> IO Type

foreign import ccall unsafe "mlirVectorTypeGet"
  vectorTypeGet :: CIntPtr -> Ptr Int64 -> Type -> IO Type
foreign import ccall unsafe "mlirRankedTensorTypeGet"
  rankedTensorTypeGet :: CIntPtr -> Ptr Int64 -> Type -> Attribute -> IO Type
foreign import ccall unsafe "mlirUnrankedTensorTypeGet"
  unrankedTensorTypeGet :: Type -> IO Type

foreign import ccall unsafe "mlirFunctionTypeGet"
  functionTypeGet :: Context -> CIntPtr -> Ptr Type -> CIntPtr -> Ptr Type -> IO Type
