{-# LANGUAGE ForeignFunctionInterface, ViewPatterns #-}
module MLIR.C.BuiltinTypes (
  mlirIntegerTypeGet
, mlirIntegerTypeSignedGet
, mlirIntegerTypeUnsignedGet
, mlirIndexTypeGet

, mlirFloat8E5M2TypeGet
, mlirFloat8E4M3FNTypeGet
, mlirFloat8E5M2FNUZTypeGet
, mlirFloat8E4M3FNUZTypeGet
, mlirFloat8E4M3B11FNUZTypeGet
, mlirBF16TypeGet
, mlirF16TypeGet
, mlirF32TypeGet
, mlirF64TypeGet
-- , mlirTF32TypeGet
, mlirNoneTypeGet

, mlirComplexTypeGet
, mlirVectorTypeGet
, mlirRankedTensorTypeGet
, mlirUnrankedTensorTypeGet
, mlirMemRefTypeGet
, mlirMemRefTypeContiguousGet
, mlirUnrankedMemRefTypeGet

, mlirTupleTypeGet
, mlirFunctionTypeGet
) where
import MLIR.C.IR

import Foreign
import Foreign.C


foreign import ccall unsafe "mlirIntegerTypeGet" 
  mlirIntegerTypeGet :: Context -> CUInt -> IO Type
foreign import ccall unsafe "mlirIntegerTypeSignedGet"
  mlirIntegerTypeSignedGet :: Context -> CUInt -> IO Type
foreign import ccall unsafe "mlirIntegerTypeUnsignedGet"
  mlirIntegerTypeUnsignedGet :: Context -> CUInt -> IO Type
foreign import ccall unsafe "mlirIndexTypeGet" 
  mlirIndexTypeGet :: Context -> IO Type


foreign import ccall unsafe "mlirFloat8E5M2TypeGet"
  mlirFloat8E5M2TypeGet :: Context -> IO Type
foreign import ccall unsafe "mlirFloat8E4M3FNTypeGet"
  mlirFloat8E4M3FNTypeGet :: Context -> IO Type
foreign import ccall unsafe "mlirFloat8E5M2FNUZTypeGet"
  mlirFloat8E5M2FNUZTypeGet :: Context -> IO Type
foreign import ccall unsafe "mlirFloat8E4M3FNUZTypeGet"
  mlirFloat8E4M3FNUZTypeGet :: Context -> IO Type
foreign import ccall unsafe "mlirFloat8E4M3B11FNUZTypeGet"
  mlirFloat8E4M3B11FNUZTypeGet :: Context -> IO Type
foreign import ccall unsafe "mlirBF16TypeGet"
  mlirBF16TypeGet :: Context -> IO Type
foreign import ccall unsafe "mlirF16TypeGet"
  mlirF16TypeGet :: Context -> IO Type
foreign import ccall unsafe "mlirF32TypeGet"
  mlirF32TypeGet :: Context -> IO Type
foreign import ccall unsafe "mlirF64TypeGet"
  mlirF64TypeGet :: Context -> IO Type
-- foreign import ccall unsafe "mlirTF32TypeGet"
--   mlirTF32TypeGet :: Context -> IO Type
foreign import ccall unsafe "mlirNoneTypeGet"
  mlirNoneTypeGet :: Context -> IO Type

foreign import ccall unsafe "mlirComplexTypeGet"
  mlirComplexTypeGet :: Type -> IO Type
foreign import ccall unsafe "mlirVectorTypeGet"
  mlirVectorTypeGet' :: CIntPtr -> Ptr Int64 -> Type -> IO Type
mlirVectorTypeGet :: [Int64] -> Type -> IO Type
mlirVectorTypeGet shape elementType = withArrayLen shape $ \(fromIntegral -> rank) shapePtr ->
  mlirVectorTypeGet' rank shapePtr elementType

foreign import ccall unsafe "mlirRankedTensorTypeGet"
  mlirRankedTensorTypeGet' :: CIntPtr -> Ptr Int64 -> Type -> Attribute -> IO Type
mlirRankedTensorTypeGet :: [Int64] -> Type -> Attribute -> IO Type
mlirRankedTensorTypeGet shape elementType encoding = withArrayLen shape $ \(fromIntegral -> rank) shapePtr ->
  mlirRankedTensorTypeGet' rank shapePtr elementType encoding 


foreign import ccall unsafe "mlirUnrankedTensorTypeGet" 
  mlirUnrankedTensorTypeGet :: Type -> IO Type
foreign import ccall unsafe "mlirMemRefTypeGet"
  mlirMemRefTypeGet' :: Type -> CIntPtr -> Ptr Int64 -> Attribute -> Attribute -> IO Type
mlirMemRefTypeGet :: Type -> [Int64] -> Attribute -> Attribute -> IO Type
mlirMemRefTypeGet elementType shape layout memorySpace = withArrayLen shape $ \(fromIntegral -> rank) shapePtr -> 
  mlirMemRefTypeGet' elementType rank shapePtr layout memorySpace

foreign import ccall unsafe "mlirMemRefTypeContiguousGet"
  mlirMemRefTypeContiguousGet' :: Type -> CIntPtr -> Ptr Int64 -> Attribute -> IO Type
mlirMemRefTypeContiguousGet :: Type -> [Int64] -> Attribute -> IO Type
mlirMemRefTypeContiguousGet elementType shape memorySpace = withArrayLen shape $ \(fromIntegral -> rank) shapePtr ->
  mlirMemRefTypeContiguousGet' elementType rank shapePtr memorySpace 


foreign import ccall unsafe "mlirUnrankedMemRefTypeGet"
  mlirUnrankedMemRefTypeGet :: Type -> Attribute -> IO Type

foreign import ccall unsafe "mlirTupleTypeGet"
  mlirTupleTypeGet' :: Context -> CIntPtr -> Ptr Type -> IO Type 
mlirTupleTypeGet :: Context -> [Type] -> IO Type
mlirTupleTypeGet ctx elements = withArrayLen elements $ \(fromIntegral -> numElements) elementsPtr ->
  mlirTupleTypeGet' ctx numElements elementsPtr 

foreign import ccall unsafe "mlirFunctionTypeGet"
  mlirFunctionTypeGet' :: Context -> CIntPtr -> Ptr Type -> CIntPtr -> Ptr Type -> IO Type
mlirFunctionTypeGet :: Context -> [Type] -> [Type] -> IO Type
mlirFunctionTypeGet ctx inputs results = withArrayLen inputs $ \(fromIntegral -> numInputs) inputsPtr -> 
  withArrayLen results $ \(fromIntegral -> numResults) resultsPtr ->
    mlirFunctionTypeGet' ctx numInputs inputsPtr numResults resultsPtr
-- mlirOpaqueTypeGet

