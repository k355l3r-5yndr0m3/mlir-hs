{-# LANGUAGE ForeignFunctionInterface, ViewPatterns #-}
module MLIR.C.BuiltinAttributes (
  mlirAttributeGetNull
, mlirArrayAttrGet
, mlirDictionaryAttrGet

, mlirFloatAttrDoubleGet
, mlirIntegerAttrGet
, mlirBoolAttrGet
, mlirStringAttrGet
, mlirStringTypedGet
, mlirFlatSymbolRefAttrGet

, mlirTypeAttrGet
, mlirUnitAttrGet
, mlirDenseBoolArrayGet
, mlirDenseI8ArrayGet
, mlirDenseI16ArrayGet
, mlirDenseI32ArrayGet
, mlirDenseI64ArrayGet
, mlirDenseF32ArrayGet
, mlirDenseF64ArrayGet

, mlirDenseElementsAttrGet
, mlirDenseElementsAttrSplatGet
, mlirDenseElementsAttrBoolSplatGet
, mlirDenseElementsAttrUInt8SplatGet
, mlirDenseElementsAttrInt8SplatGet
, mlirDenseElementsAttrUInt32SplatGet
, mlirDenseElementsAttrInt32SplatGet
, mlirDenseElementsAttrUInt64SplatGet
, mlirDenseElementsAttrInt64SplatGet
, mlirDenseElementsAttrFloatSplatGet
, mlirDenseElementsAttrDoubleSplatGet

, mlirDenseElementsAttrBoolGet
, mlirDenseElementsAttrUInt8Get
, mlirDenseElementsAttrInt8Get
, mlirDenseElementsAttrUInt16Get
, mlirDenseElementsAttrInt16Get
, mlirDenseElementsAttrUInt32Get
, mlirDenseElementsAttrInt32Get
, mlirDenseElementsAttrUInt64Get
, mlirDenseElementsAttrInt64Get
, mlirDenseElementsAttrFloatGet
, mlirDenseElementsAttrDoubleGet
, mlirDenseElementsAttrFloat16Get
, mlirDenseElementsAttrBFloat16Get

, mlirDenseElementsAttrReshapeGet
, mlirSparseElementsAttribute

, mlirStridedLayoutAttrGet
) where
import MLIR.C.IR

import Foreign
import Foreign.C

foreign import ccall unsafe "mlirAttributeGetNull"
  mlirAttributeGetNull :: Attribute

-- mlirAffineMapAttrGet
foreign import ccall unsafe "mlirArrayAttrGet"
  mlirArrayAttrGet' :: Context -> CIntPtr -> Ptr Attribute -> IO Attribute
mlirArrayAttrGet :: Context -> [Attribute] -> IO Attribute
mlirArrayAttrGet ctx elements = withArrayLen elements $ \(fromIntegral -> numElements) elementsPtr -> 
  mlirArrayAttrGet' ctx numElements elementsPtr

foreign import ccall unsafe "mlirDictionaryAttrGet"
  mlirDictionaryAttrGet' :: Context -> CIntPtr -> Ptr NamedAttribute -> IO Attribute
mlirDictionaryAttrGet :: Context -> [NamedAttribute] -> IO Attribute
mlirDictionaryAttrGet ctx elements = withArrayLen elements $ \(fromIntegral -> numElements) elementsPtr ->
  mlirDictionaryAttrGet' ctx numElements elementsPtr

foreign import ccall unsafe "mlirFloatAttrDoubleGet"
  mlirFloatAttrDoubleGet :: Context -> Type -> Double -> IO Attribute
foreign import ccall unsafe "mlirIntegerAttrGet"
  mlirIntegerAttrGet :: Type -> Int64 -> IO Attribute
foreign import ccall unsafe "mlirBoolAttrGet"
  mlirBoolAttrGet :: Context -> CInt -> IO Attribute
-- mlirOpaqueAttrGet

foreign import ccall unsafe "hs__mlirStringAttrGet"
  mlirStringAttrGet' :: Context -> Ptr CChar -> CSize -> IO Attribute
mlirStringAttrGet :: Context -> String -> IO Attribute
mlirStringAttrGet ctx str = withCStringLen str $ \(strPtr, fromIntegral -> strLength) ->
  mlirStringAttrGet' ctx strPtr strLength


foreign import ccall unsafe "hs__mlirStringAttrTypedGet"
  mlirStringAttrTypedGet' :: Type -> Ptr CChar -> CSize -> IO Attribute
mlirStringTypedGet :: Type -> String -> IO Attribute
mlirStringTypedGet _type str = withCStringLen str $ \(strPtr, fromIntegral -> strLength) ->
  mlirStringAttrTypedGet' _type strPtr strLength

-- mlirSymbolRefAttrGet 
foreign import ccall unsafe "hs__mlirFlatSymbolRefAttrGet"
  mlirFlatSymbolRefAttrGet' :: Context -> Ptr CChar -> CSize -> IO Attribute
mlirFlatSymbolRefAttrGet :: Context -> String -> IO Attribute
mlirFlatSymbolRefAttrGet ctx symbol = withCStringLen symbol $ \(symbolPtr, fromIntegral -> symbolLength) -> 
  mlirFlatSymbolRefAttrGet' ctx symbolPtr symbolLength


foreign import ccall unsafe "mlirTypeAttrGet"
  mlirTypeAttrGet :: Type -> IO Attribute
 
foreign import ccall unsafe "mlirUnitAttrGet"
  mlirUnitAttrGet :: Context -> IO Attribute

foreign import ccall unsafe "mlirDenseBoolArrayGet"
  mlirDenseBoolArrayGet' :: Context -> CIntPtr -> Ptr CInt -> IO Attribute
mlirDenseBoolArrayGet :: Context -> [Bool] -> IO Attribute
mlirDenseBoolArrayGet ctx values = withArrayLen (fmap (\v -> if v then 1 else 0) values) $ \(fromIntegral -> numValues) valuesPtr -> 
  mlirDenseBoolArrayGet' ctx numValues valuesPtr

foreign import ccall unsafe "mlirDenseI8ArrayGet"
  mlirDenseI8ArrayGet' :: Context -> CIntPtr -> Ptr Int8 -> IO Attribute
mlirDenseI8ArrayGet :: Context -> [Int8] -> IO Attribute
mlirDenseI8ArrayGet ctx values = withArrayLen values $ \(fromIntegral -> numValues) valuesPtr -> 
  mlirDenseI8ArrayGet' ctx numValues valuesPtr

foreign import ccall unsafe "mlirDenseI16ArrayGet"
  mlirDenseI16ArrayGet' :: Context -> CIntPtr -> Ptr Int16 -> IO Attribute
mlirDenseI16ArrayGet :: Context -> [Int16] -> IO Attribute
mlirDenseI16ArrayGet ctx values = withArrayLen values $ \(fromIntegral -> numValues) valuesPtr -> 
  mlirDenseI16ArrayGet' ctx numValues valuesPtr

foreign import ccall unsafe "mlirDenseI32ArrayGet"
  mlirDenseI32ArrayGet' :: Context -> CIntPtr -> Ptr Int32 -> IO Attribute 
mlirDenseI32ArrayGet :: Context -> [Int32] -> IO Attribute
mlirDenseI32ArrayGet ctx values = withArrayLen values $ \(fromIntegral -> numValues) valuesPtr -> 
  mlirDenseI32ArrayGet' ctx numValues valuesPtr

foreign import ccall unsafe "mlirDenseI64ArrayGet"
  mlirDenseI64ArrayGet' :: Context -> CIntPtr -> Ptr Int64 -> IO Attribute
mlirDenseI64ArrayGet :: Context -> [Int64] -> IO Attribute
mlirDenseI64ArrayGet ctx values = withArrayLen values $ \(fromIntegral -> numValues) valuesPtr -> 
  mlirDenseI64ArrayGet' ctx numValues valuesPtr

foreign import ccall unsafe "mlirDenseF32ArrayGet"
  mlirDenseF32ArrayGet' :: Context -> CIntPtr -> Ptr Float -> IO Attribute
mlirDenseF32ArrayGet :: Context -> [Float] -> IO Attribute
mlirDenseF32ArrayGet ctx values = withArrayLen values $ \(fromIntegral -> numValues) valuesPtr ->
  mlirDenseF32ArrayGet' ctx numValues valuesPtr

foreign import ccall unsafe "mlirDenseF64ArrayGet"
  mlirDenseF64ArrayGet' :: Context -> CIntPtr -> Ptr Double -> IO Attribute
mlirDenseF64ArrayGet :: Context -> [Double] -> IO Attribute
mlirDenseF64ArrayGet ctx values = withArrayLen values $ \(fromIntegral -> numValues) valuesPtr ->
  mlirDenseF64ArrayGet' ctx numValues valuesPtr


foreign import ccall unsafe "mlirDenseElementsAttrGet "
  mlirDenseElementsAttrGet' :: Type -> CIntPtr -> Ptr Attribute -> IO Attribute
mlirDenseElementsAttrGet :: Type -> [Attribute] -> IO Attribute
mlirDenseElementsAttrGet ctx values = withArrayLen values $ \(fromIntegral -> numValues) valuesPtr ->
  mlirDenseElementsAttrGet' ctx numValues valuesPtr

-- mlirDenseElementsAttrRawBufferGet
foreign import ccall unsafe "mlirDenseElementsAttrSplatGet"
  mlirDenseElementsAttrSplatGet :: Type -> Attribute -> IO Attribute
foreign import ccall unsafe "mlirDenseElementsAttrBoolSplatGet"
  mlirDenseElementsAttrBoolSplatGet' :: Type -> CBool -> IO Attribute
mlirDenseElementsAttrBoolSplatGet :: Type -> Bool -> IO Attribute
mlirDenseElementsAttrBoolSplatGet ctx element = 
  mlirDenseElementsAttrBoolSplatGet' ctx (if element then 1 else 0)
foreign import ccall unsafe "mlirDenseElementsAttrUInt8SplatGet" 
  mlirDenseElementsAttrUInt8SplatGet :: Type -> Word8 -> IO Attribute
foreign import ccall unsafe "mlirDenseElementsAttrInt8SplatGet"
  mlirDenseElementsAttrInt8SplatGet :: Type -> Int8 -> IO Attribute
foreign import ccall unsafe "mlirDenseElementsAttrUInt32SplatGet"
  mlirDenseElementsAttrUInt32SplatGet :: Type -> Word32 -> IO Attribute
foreign import ccall unsafe "mlirDenseElementsAttrInt32SplatGet"
  mlirDenseElementsAttrInt32SplatGet :: Type -> Int32 -> IO Attribute
foreign import ccall unsafe "mlirDenseElementsAttrUInt64SplatGet"
  mlirDenseElementsAttrUInt64SplatGet :: Type -> Word64 -> IO Attribute
foreign import ccall unsafe "mlirDenseElementsAttrInt64SplatGet"
  mlirDenseElementsAttrInt64SplatGet :: Type -> Int64 -> IO Attribute
foreign import ccall unsafe "mlirDenseElementsAttrFloatSplatGet"
  mlirDenseElementsAttrFloatSplatGet :: Type -> Float -> IO Attribute
foreign import ccall unsafe "mlirDenseElementsAttrDoubleSplatGet"
  mlirDenseElementsAttrDoubleSplatGet :: Type -> Double -> IO Attribute


foreign import ccall unsafe "mlirDenseElementsAttrBoolGet"
  mlirDenseElementsAttrBoolGet' :: Type -> CIntPtr -> Ptr CInt -> IO Attribute
mlirDenseElementsAttrBoolGet :: Type -> [Bool] -> IO Attribute
mlirDenseElementsAttrBoolGet shapeType (fmap (\x -> if x then 1 else 0) -> elements) = withArrayLen elements $ \(fromIntegral -> numElements) elementsPtr -> 
  mlirDenseElementsAttrBoolGet' shapeType numElements elementsPtr


foreign import ccall unsafe "mlirDenseElementsAttrUInt8Get"
  mlirDenseElementsAttrUInt8Get' :: Type -> CIntPtr -> Ptr Word8 -> IO Attribute
mlirDenseElementsAttrUInt8Get :: Type -> [Word8] -> IO Attribute
mlirDenseElementsAttrUInt8Get shapeType elements = withArrayLen elements $ \(fromIntegral -> numElements) elementsPtr ->
  mlirDenseElementsAttrUInt8Get' shapeType numElements elementsPtr


foreign import ccall unsafe "mlirDenseElementsAttrInt8Get"
  mlirDenseElementsAttrInt8Get' :: Type -> CIntPtr -> Ptr Int8 -> IO Attribute
mlirDenseElementsAttrInt8Get :: Type -> [Int8] -> IO Attribute
mlirDenseElementsAttrInt8Get shapeType elements = withArrayLen elements $ \(fromIntegral -> numElements) elementsPtr ->
  mlirDenseElementsAttrInt8Get' shapeType numElements elementsPtr



foreign import ccall unsafe "mlirDenseElementsAttrUInt16Get"
  mlirDenseElementsAttrUInt16Get' :: Type -> CIntPtr -> Ptr Word16 -> IO Attribute
mlirDenseElementsAttrUInt16Get :: Type -> [Word16] -> IO Attribute
mlirDenseElementsAttrUInt16Get shapeType elements = withArrayLen elements $ \(fromIntegral -> numElements) elementsPtr ->
  mlirDenseElementsAttrUInt16Get' shapeType numElements elementsPtr



foreign import ccall unsafe "mlirDenseElementsAttrInt16Get"
  mlirDenseElementsAttrInt16Get' :: Type -> CIntPtr -> Ptr Int16 -> IO Attribute
mlirDenseElementsAttrInt16Get :: Type -> [Int16] -> IO Attribute
mlirDenseElementsAttrInt16Get shapeType elements = withArrayLen elements $ \(fromIntegral -> numElements) elementsPtr ->
  mlirDenseElementsAttrInt16Get' shapeType numElements elementsPtr



foreign import ccall unsafe "mlirDenseElementsAttrUInt32Get"
  mlirDenseElementsAttrUInt32Get' :: Type -> CIntPtr -> Ptr Word32 -> IO Attribute
mlirDenseElementsAttrUInt32Get :: Type -> [Word32] -> IO Attribute
mlirDenseElementsAttrUInt32Get shapeType elements = withArrayLen elements $ \(fromIntegral -> numElements) elementsPtr ->
  mlirDenseElementsAttrUInt32Get' shapeType numElements elementsPtr


foreign import ccall unsafe "mlirDenseElementsAttrInt32Get"
  mlirDenseElementsAttrInt32Get' :: Type -> CIntPtr -> Ptr Int32 -> IO Attribute
mlirDenseElementsAttrInt32Get :: Type -> [Int32] -> IO Attribute
mlirDenseElementsAttrInt32Get shapeType elements = withArrayLen elements $ \(fromIntegral -> numElements) elementsPtr ->
  mlirDenseElementsAttrInt32Get' shapeType numElements elementsPtr


foreign import ccall unsafe "mlirDenseElementsAttrUInt64Get"
  mlirDenseElementsAttrUInt64Get' :: Type -> CIntPtr -> Ptr Word64 -> IO Attribute
mlirDenseElementsAttrUInt64Get :: Type -> [Word64] -> IO Attribute
mlirDenseElementsAttrUInt64Get shapeType elements = withArrayLen elements $ \(fromIntegral -> numElements) elementsPtr ->
  mlirDenseElementsAttrUInt64Get' shapeType numElements elementsPtr


foreign import ccall unsafe "mlirDenseElementsAttrInt64Get"
  mlirDenseElementsAttrInt64Get' :: Type -> CIntPtr -> Ptr Int64 -> IO Attribute
mlirDenseElementsAttrInt64Get :: Type -> [Int64] -> IO Attribute
mlirDenseElementsAttrInt64Get shapeType elements = withArrayLen elements $ \(fromIntegral -> numElements) elementsPtr ->
  mlirDenseElementsAttrInt64Get' shapeType numElements elementsPtr



foreign import ccall unsafe "mlirDenseElementsAttrFloatGet"
  mlirDenseElementsAttrFloatGet' :: Type -> CIntPtr -> Ptr Float -> IO Attribute
mlirDenseElementsAttrFloatGet :: Type -> [Float] -> IO Attribute
mlirDenseElementsAttrFloatGet shapeType elements = withArrayLen elements $ \(fromIntegral -> numElements) elementsPtr ->
  mlirDenseElementsAttrFloatGet' shapeType numElements elementsPtr



foreign import ccall unsafe "mlirDenseElementsAttrDoubleGet"
  mlirDenseElementsAttrDoubleGet' :: Type -> CIntPtr -> Ptr Double -> IO Attribute
mlirDenseElementsAttrDoubleGet :: Type -> [Double] -> IO Attribute
mlirDenseElementsAttrDoubleGet shapeType elements = withArrayLen elements $ \(fromIntegral -> numElements) elementsPtr ->
  mlirDenseElementsAttrDoubleGet' shapeType numElements elementsPtr


foreign import ccall unsafe "mlirDenseElementsAttrBFloat16Get"
  mlirDenseElementsAttrBFloat16Get' :: Type -> CIntPtr -> Ptr Word16 -> IO Attribute
mlirDenseElementsAttrBFloat16Get :: Type -> [Word16] -> IO Attribute
mlirDenseElementsAttrBFloat16Get shapeType elements = withArrayLen elements $ \(fromIntegral -> numElements) elementsPtr ->
  mlirDenseElementsAttrBFloat16Get' shapeType numElements elementsPtr


foreign import ccall unsafe "mlirDenseElementsAttrFloat16Get"
  mlirDenseElementsAttrFloat16Get' :: Type -> CIntPtr -> Ptr Word16 -> IO Attribute
mlirDenseElementsAttrFloat16Get :: Type -> [Word16] -> IO Attribute
mlirDenseElementsAttrFloat16Get shapeType elements = withArrayLen elements $ \(fromIntegral -> numElements) elementsPtr ->
  mlirDenseElementsAttrFloat16Get' shapeType numElements elementsPtr

-- mlirDenseElementsAttrStringGet

foreign import ccall unsafe "mlirDenseElementsAttrReshapeGet"
  mlirDenseElementsAttrReshapeGet :: Attribute -> Type -> IO Attribute

-- mlirUnmanaged*

-- This does not ends in a "Get" but it probably will be renamed later
foreign import ccall unsafe "mlirSparseElementsAttribute"
  mlirSparseElementsAttribute :: Type -> Attribute -> Attribute -> IO Attribute

foreign import ccall unsafe "mlirStridedLayoutAttrGet"
  mlirStridedLayoutAttrGet' :: Context -> Int64 -> CIntPtr -> Ptr Int64 -> IO Attribute
mlirStridedLayoutAttrGet :: Context -> Int64 -> [Int64] -> IO Attribute
mlirStridedLayoutAttrGet ctx offset strides = withArrayLen strides $ \(fromIntegral -> numStrides) stridesPtr -> 
  mlirStridedLayoutAttrGet' ctx offset numStrides stridesPtr


{-
foreign import ccall unsafe "FUNC"
  FUNC' :: Type -> CIntPtr -> Ptr TYPE -> IO Attribute
FUNC :: Type -> [TYPE] -> IO Attribute
FUNC shapeType elements = withArrayLen elements $ \(fromIntegral -> numElements) elementsPtr ->
  FUNC' shapeType numElements elementsPtr
-}

