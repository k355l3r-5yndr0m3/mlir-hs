{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module MLIR.C.BuiltinAttributes where
import MLIR.C.IR
import MLIR.C.AffineMap

import Foreign
import Foreign.C 
import GHC.Exts (ByteArray#)

foreign import ccall unsafe "mlirAttributeGetNull"
  attributeGetNull :: Attribute

foreign import ccall unsafe "mlirAffineMapAttrGet"
  affineMapAttrGet :: AffineMap -> IO Attribute

foreign import ccall unsafe "mlirArrayAttrGet"
  arrayAttrGet :: Context -> CIntPtr -> Ptr Attribute -> IO Attribute

foreign import ccall unsafe "mlirDictionaryAttrGet"
  dictionaryAttrGet :: Context -> CIntPtr -> Ptr NamedAttribute -> IO Attribute

foreign import ccall unsafe "hs__mlirFlatSymbolRefAttrGet"
  flatSymbolRefAttrGet :: Context -> CString -> CSize -> IO Attribute

foreign import ccall unsafe "mlirFloatAttrDoubleGet"
  floatAttrDoubleGet :: Context -> Type -> Double -> IO Attribute

foreign import ccall unsafe "mlirIntegerAttrGet"
  integerAttrGet :: Type -> Int64 -> IO Attribute

foreign import ccall unsafe "mlirBoolAttrGet"
  boolAttrGet :: Context -> CInt -> IO Attribute

foreign import ccall unsafe "hs__mlirStringAttrGet"
  stringAttrGet :: Context -> CString -> CSize -> IO Attribute

foreign import ccall unsafe "mlirTypeAttrGet"
  typeAttrGet :: Type -> IO Attribute

foreign import ccall unsafe "mlirUnitAttrGet"
  unitAttrGet :: Context -> IO Attribute

-- Dense array
foreign import ccall unsafe "mlirDenseBoolArrayGet"
  denseBoolArrayGet :: Context -> CIntPtr -> Ptr CInt -> IO Attribute
foreign import ccall unsafe "mlirDenseI8ArrayGet"
  denseI8ArrayGet :: Context -> CIntPtr -> Ptr Int8 -> IO Attribute
foreign import ccall unsafe "mlirDenseI16ArrayGet"
  denseI16ArrayGet :: Context -> CIntPtr -> Ptr Int16 -> IO Attribute
foreign import ccall unsafe "mlirDenseI32ArrayGet"
  denseI32ArrayGet :: Context -> CIntPtr -> Ptr Int32 -> IO Attribute 
foreign import ccall unsafe "mlirDenseI64ArrayGet"
  denseI64ArrayGet :: Context -> CIntPtr -> Ptr Int64 -> IO Attribute
foreign import ccall unsafe "mlirDenseF32ArrayGet"
  denseF32ArrayGet :: Context -> CIntPtr -> Ptr Float -> IO Attribute
foreign import ccall unsafe "mlirDenseF64ArrayGet"
  denseF64ArrayGet :: Context -> CIntPtr -> Ptr Double -> IO Attribute

-- Dense elements
foreign import ccall unsafe "mlirDenseElementsAttrRawBufferGet"
  denseElementsAttrRawBufferGet :: Type -> CSize -> ByteArray# -> IO Attribute

foreign import ccall unsafe "mlirDenseElementsAttrGet"
  denseElementsAttrGet :: Type -> CIntPtr -> Ptr Attribute -> IO Attribute
foreign import ccall unsafe "mlirDenseElementsAttrSplatGet"
  denseElementsAttrSplatGet :: Type -> Attribute -> IO Attribute

foreign import ccall unsafe "mlirDenseElementsAttrBoolGet"
  denseElementsAttrBoolGet :: Type -> CIntPtr -> Ptr CInt -> IO Attribute
foreign import ccall unsafe "mlirDenseElementsAttrBoolSplatGet"
  denseElementsAttrBoolSplatGet :: Type -> CBool -> IO Attribute

foreign import ccall unsafe "mlirDenseElementsAttrUInt8Get"
  denseElementsAttrUInt8Get :: Type -> CIntPtr -> Ptr Word8 -> IO Attribute
foreign import ccall unsafe "mlirDenseElementsAttrUInt8SplatGet"
  denseElementsAttrUInt8SplatGet :: Type -> Word8 -> IO Attribute

foreign import ccall unsafe "mlirDenseElementsAttrInt8Get"
  denseElementsAttrInt8Get :: Type -> CIntPtr -> Ptr Int8 -> IO Attribute
foreign import ccall unsafe "mlirDenseElementsAttrInt8SplatGet"
  denseElementsAttrInt8SplatGet :: Type -> Int8 -> IO Attribute

foreign import ccall unsafe "mlirDenseElementsAttrUInt16Get"
  denseElementsAttrUInt16Get :: Type -> CIntPtr -> Ptr Word16 -> IO Attribute

foreign import ccall unsafe "mlirDenseElementsAttrInt16Get"
  denseElementsAttrInt16Get :: Type -> CIntPtr -> Ptr Int16 -> IO Attribute

foreign import ccall unsafe "mlirDenseElementsAttrUInt32Get"
  denseElementsAttrUInt32Get :: Type -> CIntPtr -> Ptr Word32 -> IO Attribute
foreign import ccall unsafe "mlirDenseElementsAttrUInt32SplatGet"
  denseElementsAttrUInt32SplatGet :: Type -> Word32 -> IO Attribute

foreign import ccall unsafe "mlirDenseElementsAttrInt32Get"
  denseElementsAttrInt32Get :: Type -> CIntPtr -> Ptr Int32 -> IO Attribute
foreign import ccall unsafe "mlirDenseElementsAttrInt32SplatGet"
  denseElementsAttrInt32SplatGet :: Type -> Int32 -> IO Attribute

foreign import ccall unsafe "mlirDenseElementsAttrUInt64Get"
  denseElementsAttrUInt64Get :: Type -> CIntPtr -> Ptr Word64 -> IO Attribute
foreign import ccall unsafe "mlirDenseElementsAttrUInt64SplatGet"
  denseElementsAttrUInt64SplatGet :: Type -> Word64 -> IO Attribute

foreign import ccall unsafe "mlirDenseElementsAttrInt64Get"
  denseElementsAttrInt64Get :: Type -> CIntPtr -> Ptr Int64 -> IO Attribute
foreign import ccall unsafe "mlirDenseElementsAttrInt64SplatGet"
  denseElementsAttrInt64SplatGet :: Type -> Int64 -> IO Attribute

foreign import ccall unsafe "mlirDenseElementsAttrFloatGet"
  denseElementsAttrFloatGet :: Type -> CIntPtr -> Ptr Float -> IO Attribute
foreign import ccall unsafe "mlirDenseElementsAttrFloatSplatGet"
  denseElementsAttrFloatSplatGet :: Type -> Float -> IO Attribute

foreign import ccall unsafe "mlirDenseElementsAttrDoubleGet"
  denseElementsAttrDoubleGet :: Type -> CIntPtr -> Ptr Double -> IO Attribute
foreign import ccall unsafe "mlirDenseElementsAttrDoubleSplatGet"
  denseElementsAttrDoubleSplatGet :: Type -> Double -> IO Attribute

foreign import ccall unsafe "mlirDenseElementsAttrBFloat16Get"
  denseElementsAttrBFloat16Get :: Type -> CIntPtr -> Ptr Word16 -> IO Attribute

foreign import ccall unsafe "mlirDenseElementsAttrFloat16Get"
  denseElementsAttrFloat16Get :: Type -> CIntPtr -> Ptr Word16 -> IO Attribute

-- TODO: implement mlirDenseElementsAttrStringGet
