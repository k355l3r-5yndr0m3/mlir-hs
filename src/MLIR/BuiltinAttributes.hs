{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
module MLIR.BuiltinAttributes where

import qualified MLIR.C.BuiltinAttributes as C

import MLIR.IR
import MLIR.BuiltinTypes

import Control.Monad

import Data.String

import Foreign
import Foreign.C

import GHC.IsList

import Data.Bool
import Data.Primitive.ByteArray


data NullAttr = NullAttr
instance AttrGet NullAttr where
  attrGet _ _ = return C.attributeGetNull
data UnitAttr
instance AttrGet UnitAttr where
  attrGet _ = C.unitAttrGet

data IntegerAttr = IntegerAttr IntegerType Int64
instance AttrGet IntegerAttr where
  attrGet (IntegerAttr t v) c = (`C.integerAttrGet` v) =<< typeGet t c
newtype BoolAttr = BoolAttr Bool
instance AttrGet BoolAttr where
  attrGet (BoolAttr boolean) = attrGet (IntegerAttr (IntegerType Signless 1) (if boolean then 1 else 0))

data FloatAttr a = FloatAttr a Double
instance TypeGet a => AttrGet (FloatAttr a) where
  attrGet (FloatAttr a v) c = do 
    a' <- typeGet a c 
    C.floatAttrDoubleGet c a' v


newtype ArrayAttr = ArrayAttr [AnyAttr]
instance AttrGet ArrayAttr where
  attrGet (ArrayAttr attrs) c = do
    attrs' <- mapM (`attrGet` c) attrs
    withArrayLen attrs' $ \ (fromIntegral -> numElements) elements -> 
      C.arrayAttrGet c numElements elements
instance IsList ArrayAttr where
  type Item ArrayAttr = AnyAttr
  toList (ArrayAttr l) = l
  fromList = ArrayAttr


newtype DenseArray a = DenseArray [a]

type DenseBoolArrayAttr = DenseArray Bool
type DenseI8ArrayAttr = DenseArray Int8
type DenseI16ArrayAttr = DenseArray Int16
type DenseI32ArrayAttr = DenseArray Int32
type DenseI64ArrayAttr = DenseArray Int64
type DenseF32ArrayAttr = DenseArray Float
type DenseF64ArrayAttr = DenseArray Double

instance AttrGet DenseBoolArrayAttr where
  attrGet (DenseArray (fmap (bool 0 1) -> a :: [CInt])) c = 
    withArrayLen a $ \ (fromIntegral -> size) values -> 
      C.denseBoolArrayGet c size values
instance AttrGet DenseI8ArrayAttr where
  attrGet (DenseArray a) c = 
    withArrayLen a $ \ (fromIntegral -> size) values -> 
      C.denseI8ArrayGet c size values
instance AttrGet DenseI16ArrayAttr where
  attrGet (DenseArray a) c = 
    withArrayLen a $ \ (fromIntegral -> size) values -> 
      C.denseI16ArrayGet c size values
instance AttrGet DenseI32ArrayAttr where
  attrGet (DenseArray a) c = 
    withArrayLen a $ \ (fromIntegral -> size) values -> 
      C.denseI32ArrayGet c size values
instance AttrGet DenseI64ArrayAttr where
  attrGet (DenseArray a) c = 
    withArrayLen a $ \ (fromIntegral -> size) values -> 
      C.denseI64ArrayGet c size values
instance AttrGet DenseF32ArrayAttr where
  attrGet (DenseArray a) c = 
    withArrayLen a $ \ (fromIntegral -> size) values -> 
      C.denseF32ArrayGet c size values
instance AttrGet DenseF64ArrayAttr where
  attrGet (DenseArray a) c = 
    withArrayLen a $ \ (fromIntegral -> size) values -> 
      C.denseF64ArrayGet c size values

class AttrGet a => DenseArrayAttr a
instance DenseArrayAttr DenseBoolArrayAttr
instance DenseArrayAttr DenseI8ArrayAttr
instance DenseArrayAttr DenseI16ArrayAttr 
instance DenseArrayAttr DenseI32ArrayAttr
instance DenseArrayAttr DenseI64ArrayAttr
instance DenseArrayAttr DenseF32ArrayAttr
instance DenseArrayAttr DenseF64ArrayAttr

newtype StringAttr = StringAttr String
instance AttrGet StringAttr where
  attrGet (StringAttr str) c =
    withCStringLen str $ \ (str', fromIntegral -> strSize) -> 
      C.stringAttrGet c str' strSize
instance IsString StringAttr where
  fromString = StringAttr

newtype TypeAttr a = TypeAttr a
instance TypeGet a => AttrGet (TypeAttr a) where
  attrGet (TypeAttr t) = C.typeAttrGet <=< typeGet t
 

newtype FlatSymbolRefAttr = FlatSymbolRefAttr String
instance AttrGet FlatSymbolRefAttr where
  attrGet (FlatSymbolRefAttr symbol) c = 
    withCStringLen symbol $ \ (str, fromIntegral -> size) -> 
      C.flatSymbolRefAttrGet c str size

-- Typed attributes
class AttrGet a => TypedAttr a
-- TODO: Implement


-- Dense elements
data DenseIntOrFPElements s t = DenseIntOrFPElements s t
instance ShapedType (s IntegerType) => AttrGet (DenseIntOrFPElements (s IntegerType) Bool) where
  attrGet (DenseIntOrFPElements s (bool 0 1 -> t)) c = do 
    s' <- typeGet s c
    C.denseElementsAttrBoolSplatGet s' t
instance ShapedType (s IntegerType) => AttrGet (DenseIntOrFPElements (s IntegerType) [Bool]) where
  attrGet (DenseIntOrFPElements s (fmap $ bool 0 1 -> t)) c = do 
    s' <- typeGet s c
    withArrayLen t $ \ (fromIntegral -> n) t' ->
      C.denseElementsAttrBoolGet s' n t'

instance ShapedType (s IntegerType) => AttrGet (DenseIntOrFPElements (s IntegerType) Word8) where
  attrGet (DenseIntOrFPElements s t) c = do 
    s' <- typeGet s c
    C.denseElementsAttrUInt8SplatGet s' t
instance ShapedType (s IntegerType) => AttrGet (DenseIntOrFPElements (s IntegerType) [Word8]) where
  attrGet (DenseIntOrFPElements s t) c = do 
    s' <- typeGet s c
    withArrayLen t $ \ (fromIntegral -> n) t' ->
      C.denseElementsAttrUInt8Get s' n t'

instance ShapedType (s IntegerType) => AttrGet (DenseIntOrFPElements (s IntegerType) Int8) where
  attrGet (DenseIntOrFPElements s t) c = do 
    s' <- typeGet s c
    C.denseElementsAttrInt8SplatGet s' t
instance ShapedType (s IntegerType) => AttrGet (DenseIntOrFPElements (s IntegerType) [Int8]) where
  attrGet (DenseIntOrFPElements s t) c = do 
    s' <- typeGet s c
    withArrayLen t $ \ (fromIntegral -> n) t' ->
      C.denseElementsAttrInt8Get s' n t'

instance ShapedType (s IntegerType) => AttrGet (DenseIntOrFPElements (s IntegerType) [Word16]) where
  attrGet (DenseIntOrFPElements s t) c = do 
    s' <- typeGet s c
    withArrayLen t $ \ (fromIntegral -> n) t' ->
      C.denseElementsAttrUInt16Get s' n t'

instance ShapedType (s IntegerType) => AttrGet (DenseIntOrFPElements (s IntegerType) [Int16]) where
  attrGet (DenseIntOrFPElements s t) c = do 
    s' <- typeGet s c
    withArrayLen t $ \ (fromIntegral -> n) t' ->
      C.denseElementsAttrInt16Get s' n t'

instance ShapedType (s IntegerType) => AttrGet (DenseIntOrFPElements (s IntegerType) Word32) where
  attrGet (DenseIntOrFPElements s t) c = do 
    s' <- typeGet s c
    C.denseElementsAttrUInt32SplatGet s' t
instance ShapedType (s IntegerType) => AttrGet (DenseIntOrFPElements (s IntegerType) [Word32]) where
  attrGet (DenseIntOrFPElements s t) c = do 
    s' <- typeGet s c
    withArrayLen t $ \ (fromIntegral -> n) t' ->
      C.denseElementsAttrUInt32Get s' n t'

instance ShapedType (s IntegerType) => AttrGet (DenseIntOrFPElements (s IntegerType) Int32) where
  attrGet (DenseIntOrFPElements s t) c = do 
    s' <- typeGet s c
    C.denseElementsAttrInt32SplatGet s' t
instance ShapedType (s IntegerType) => AttrGet (DenseIntOrFPElements (s IntegerType) [Int32]) where
  attrGet (DenseIntOrFPElements s t) c = do 
    s' <- typeGet s c
    withArrayLen t $ \ (fromIntegral -> n) t' ->
      C.denseElementsAttrInt32Get s' n t'

instance ShapedType (s IntegerType) => AttrGet (DenseIntOrFPElements (s IntegerType) Word64) where
  attrGet (DenseIntOrFPElements s t) c = do 
    s' <- typeGet s c
    C.denseElementsAttrUInt64SplatGet s' t
instance ShapedType (s IntegerType) => AttrGet (DenseIntOrFPElements (s IntegerType) [Word64]) where
  attrGet (DenseIntOrFPElements s t) c = do 
    s' <- typeGet s c
    withArrayLen t $ \ (fromIntegral -> n) t' ->
      C.denseElementsAttrUInt64Get s' n t'

instance ShapedType (s IntegerType) => AttrGet (DenseIntOrFPElements (s IntegerType) Int64) where
  attrGet (DenseIntOrFPElements s t) c = do 
    s' <- typeGet s c
    C.denseElementsAttrInt64SplatGet s' t
instance ShapedType (s IntegerType) => AttrGet (DenseIntOrFPElements (s IntegerType) [Int64]) where
  attrGet (DenseIntOrFPElements s t) c = do 
    s' <- typeGet s c
    withArrayLen t $ \ (fromIntegral -> n) t' ->
      C.denseElementsAttrInt64Get s' n t'

instance ShapedType (s F32Type) => AttrGet (DenseIntOrFPElements (s F32Type) Float) where
  attrGet (DenseIntOrFPElements s t) c = do 
    s' <- typeGet s c
    C.denseElementsAttrFloatSplatGet s' t
instance ShapedType (s F32Type) => AttrGet (DenseIntOrFPElements (s F32Type) [Float]) where
  attrGet (DenseIntOrFPElements s t) c = do 
    s' <- typeGet s c
    withArrayLen t $ \ (fromIntegral -> n) t' ->
      C.denseElementsAttrFloatGet s' n t'

instance ShapedType (s F64Type) => AttrGet (DenseIntOrFPElements (s F64Type) Double) where
  attrGet (DenseIntOrFPElements s t) c = do 
    s' <- typeGet s c
    C.denseElementsAttrDoubleSplatGet s' t
instance ShapedType (s F64Type) => AttrGet (DenseIntOrFPElements (s F64Type) [Double]) where
  attrGet (DenseIntOrFPElements s t) c = do 
    s' <- typeGet s c
    withArrayLen t $ \ (fromIntegral -> n) t' ->
      C.denseElementsAttrDoubleGet s' n t'


class AttrGet a => DenseIntOrFPElementsAttr a
instance ShapedType (s IntegerType) => DenseIntOrFPElementsAttr (DenseIntOrFPElements (s IntegerType) Bool)
instance ShapedType (s IntegerType) => DenseIntOrFPElementsAttr (DenseIntOrFPElements (s IntegerType) [Bool]) 

instance ShapedType (s IntegerType) => DenseIntOrFPElementsAttr (DenseIntOrFPElements (s IntegerType) Word8) 
instance ShapedType (s IntegerType) => DenseIntOrFPElementsAttr (DenseIntOrFPElements (s IntegerType) [Word8]) 

instance ShapedType (s IntegerType) => DenseIntOrFPElementsAttr (DenseIntOrFPElements (s IntegerType) Int8) 
instance ShapedType (s IntegerType) => DenseIntOrFPElementsAttr (DenseIntOrFPElements (s IntegerType) [Int8]) 

instance ShapedType (s IntegerType) => DenseIntOrFPElementsAttr (DenseIntOrFPElements (s IntegerType) [Word16]) 

instance ShapedType (s IntegerType) => DenseIntOrFPElementsAttr (DenseIntOrFPElements (s IntegerType) [Int16]) 

instance ShapedType (s IntegerType) => DenseIntOrFPElementsAttr (DenseIntOrFPElements (s IntegerType) Word32) 
instance ShapedType (s IntegerType) => DenseIntOrFPElementsAttr (DenseIntOrFPElements (s IntegerType) [Word32]) 

instance ShapedType (s IntegerType) => DenseIntOrFPElementsAttr (DenseIntOrFPElements (s IntegerType) Int32) 
instance ShapedType (s IntegerType) => DenseIntOrFPElementsAttr (DenseIntOrFPElements (s IntegerType) [Int32]) 

instance ShapedType (s IntegerType) => DenseIntOrFPElementsAttr (DenseIntOrFPElements (s IntegerType) Word64) 
instance ShapedType (s IntegerType) => DenseIntOrFPElementsAttr (DenseIntOrFPElements (s IntegerType) [Word64]) 

instance ShapedType (s IntegerType) => DenseIntOrFPElementsAttr (DenseIntOrFPElements (s IntegerType) Int64) 
instance ShapedType (s IntegerType) => DenseIntOrFPElementsAttr (DenseIntOrFPElements (s IntegerType) [Int64]) 

instance ShapedType (s F32Type) => DenseIntOrFPElementsAttr (DenseIntOrFPElements (s F32Type) Float) 
instance ShapedType (s F32Type) => DenseIntOrFPElementsAttr (DenseIntOrFPElements (s F32Type) [Float]) 

instance ShapedType (s F64Type)     => DenseIntOrFPElementsAttr (DenseIntOrFPElements (s F64Type) Double) 
instance ShapedType (s F64Type)     => DenseIntOrFPElementsAttr (DenseIntOrFPElements (s F64Type) [Double]) 

class DenseIntOrFPElementsAttr a => DenseIntElementsAttr a
instance ShapedType (s IntegerType) => DenseIntElementsAttr (DenseIntOrFPElements (s IntegerType) Bool)
instance ShapedType (s IntegerType) => DenseIntElementsAttr (DenseIntOrFPElements (s IntegerType) [Bool]) 

instance ShapedType (s IntegerType) => DenseIntElementsAttr (DenseIntOrFPElements (s IntegerType) Word8) 
instance ShapedType (s IntegerType) => DenseIntElementsAttr (DenseIntOrFPElements (s IntegerType) [Word8]) 

instance ShapedType (s IntegerType) => DenseIntElementsAttr (DenseIntOrFPElements (s IntegerType) Int8) 
instance ShapedType (s IntegerType) => DenseIntElementsAttr (DenseIntOrFPElements (s IntegerType) [Int8]) 

instance ShapedType (s IntegerType) => DenseIntElementsAttr (DenseIntOrFPElements (s IntegerType) [Word16]) 

instance ShapedType (s IntegerType) => DenseIntElementsAttr (DenseIntOrFPElements (s IntegerType) [Int16]) 

instance ShapedType (s IntegerType) => DenseIntElementsAttr (DenseIntOrFPElements (s IntegerType) Word32) 
instance ShapedType (s IntegerType) => DenseIntElementsAttr (DenseIntOrFPElements (s IntegerType) [Word32]) 

instance ShapedType (s IntegerType) => DenseIntElementsAttr (DenseIntOrFPElements (s IntegerType) Int32) 
instance ShapedType (s IntegerType) => DenseIntElementsAttr (DenseIntOrFPElements (s IntegerType) [Int32]) 

instance ShapedType (s IntegerType) => DenseIntElementsAttr (DenseIntOrFPElements (s IntegerType) Word64) 
instance ShapedType (s IntegerType) => DenseIntElementsAttr (DenseIntOrFPElements (s IntegerType) [Word64]) 

instance ShapedType (s IntegerType) => DenseIntElementsAttr (DenseIntOrFPElements (s IntegerType) Int64) 
instance ShapedType (s IntegerType) => DenseIntElementsAttr (DenseIntOrFPElements (s IntegerType) [Int64]) 

instance ShapedType (s F32Type) => DenseIntElementsAttr (DenseIntOrFPElements (s F32Type) Float) 
instance ShapedType (s F32Type) => DenseIntElementsAttr (DenseIntOrFPElements (s F32Type) [Float]) 

class DenseIntOrFPElementsAttr a => DenseFPElementsAttr a
instance ShapedType (s F64Type)     => DenseFPElementsAttr (DenseIntOrFPElements (s F64Type) Double) 
instance ShapedType (s F64Type)     => DenseFPElementsAttr (DenseIntOrFPElements (s F64Type) [Double]) 

data DenseElementsRawBuffer s = DenseElementsRawBuffer s ByteArray
instance ShapedType s => AttrGet (DenseElementsRawBuffer s) where
  attrGet (DenseElementsRawBuffer s buffer@(ByteArray buffer#)) c = do 
    s' <- typeGet s c 
    C.denseElementsAttrRawBufferGet s' size buffer#
    where size = fromIntegral $ sizeofByteArray buffer
instance ShapedType s => DenseIntOrFPElementsAttr (DenseElementsRawBuffer s)
instance ShapedType (s IntegerType) => DenseIntElementsAttr (DenseElementsRawBuffer (s IntegerType))
instance ShapedType (s F32Type) => DenseFPElementsAttr (DenseElementsRawBuffer (s F32Type))
instance ShapedType (s F64Type) => DenseFPElementsAttr (DenseElementsRawBuffer (s F64Type))

type ElementsAttr a = DenseIntOrFPElementsAttr a
type DenseElementsAttr a = DenseIntOrFPElementsAttr a
