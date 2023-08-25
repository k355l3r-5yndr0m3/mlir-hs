{-# LANGUAGE ViewPatterns #-}
module MLIR.BuiltinAttributes where
import MLIR.C.BuiltinAttributes
import MLIR.IR (Attribute(..), Type(..))
import Foreign

type TypedAttr = Attribute

attributeNull :: Attribute
attributeNull = Attribute $ \_ -> return mlirAttributeGetNull 

type ArrayAttr = Attribute
arrayAttr :: [Attribute] -> ArrayAttr
arrayAttr ((fmap (\(Attribute x) -> x)) -> arr) = Attribute $ \c -> 
  mlirArrayAttrGet c =<< sequence (fmap (\a -> a c) arr)

type TypeAttr = Attribute
typeAttr :: Type -> TypeAttr
typeAttr (Type typeGet) = Attribute $ \c ->
  mlirTypeAttrGet =<< typeGet c

type FlatSymbolRefAttr = Attribute
flatSymbolRefAttr :: String -> FlatSymbolRefAttr
flatSymbolRefAttr symbol = Attribute $ \c -> 
  mlirFlatSymbolRefAttrGet c symbol

type DenseArrayAttr = Attribute
type DenseBoolArrayAttr = Attribute
denseBoolArrayAttr :: [Bool] -> DenseBoolArrayAttr
denseBoolArrayAttr denseArr = Attribute $ \c ->
  mlirDenseBoolArrayGet c denseArr

type DenseI8ArrayAttr = DenseArrayAttr
denseI8ArrayAttr :: [Int8] -> DenseI8ArrayAttr
denseI8ArrayAttr denseArr = Attribute $ \c ->
  mlirDenseI8ArrayGet c denseArr

type DenseI16ArrayAttr = DenseArrayAttr
denseI16ArrayAttr :: [Int16] -> DenseI16ArrayAttr
denseI16ArrayAttr denseArr = Attribute $ \c ->
  mlirDenseI16ArrayGet c denseArr

type DenseI32ArrayAttr = DenseArrayAttr
denseI32ArrayAttr :: [Int32] -> DenseI32ArrayAttr
denseI32ArrayAttr denseArr = Attribute $ \c ->
  mlirDenseI32ArrayGet c denseArr

type DenseI64ArrayAttr = DenseArrayAttr
denseI64ArrayAttr :: [Int64] -> DenseI64ArrayAttr
denseI64ArrayAttr denseArr = Attribute $ \c ->
  mlirDenseI64ArrayGet c denseArr

type DenseF32ArrayAttr = DenseArrayAttr
denseF32ArrayAttr :: [Float] -> DenseF32ArrayAttr
denseF32ArrayAttr denseArr = Attribute $ \c ->
  mlirDenseF32ArrayGet c denseArr

type DenseF64ArrayAttr = DenseArrayAttr
denseF64ArrayAttr :: [Double] -> DenseF64ArrayAttr
denseF64ArrayAttr denseArr = Attribute $ \c ->
  mlirDenseF64ArrayGet c denseArr

type StringAttr = Attribute
stringAttr :: String -> StringAttr
stringAttr str = Attribute $ \c ->
  mlirStringAttrGet c str

type DenseIntElementsAttr = Attribute
type DenseElementsAttr = Attribute
denseElementsAttrBool :: Type -> [Bool] -> DenseIntElementsAttr
denseElementsAttrBool shapeType elements = Attribute $ \c -> do
  shapeType' <- getType shapeType c
  mlirDenseElementsAttrBoolGet shapeType' elements

denseElementsAttrInt8 :: Type -> [Int8] -> DenseIntElementsAttr
denseElementsAttrInt8 shapeType elements = Attribute $ \c -> do
  shapeType' <- getType shapeType c
  mlirDenseElementsAttrInt8Get shapeType' elements

denseElementsAttrInt16 :: Type -> [Int16] -> DenseIntElementsAttr
denseElementsAttrInt16 shapeType elements = Attribute $ \c -> do
  shapeType' <- getType shapeType c
  mlirDenseElementsAttrInt16Get shapeType' elements

denseElementsAttrInt32 :: Type -> [Int32] -> DenseIntElementsAttr
denseElementsAttrInt32 shapeType elements = Attribute $ \c -> do
  shapeType' <- getType shapeType c
  mlirDenseElementsAttrInt32Get shapeType' elements

denseElementsAttrInt64 :: Type -> [Int64] -> DenseIntElementsAttr
denseElementsAttrInt64 shapeType elements = Attribute $ \c -> do
  shapeType' <- getType shapeType c
  mlirDenseElementsAttrInt64Get shapeType' elements

denseElementsAttrUInt8 :: Type -> [Word8] -> DenseIntElementsAttr
denseElementsAttrUInt8 shapeType elements = Attribute $ \c -> do
  shapeType' <- getType shapeType c
  mlirDenseElementsAttrUInt8Get shapeType' elements

denseElementsAttrUInt16 :: Type -> [Word16] -> DenseIntElementsAttr
denseElementsAttrUInt16 shapeType elements = Attribute $ \c -> do
  shapeType' <- getType shapeType c
  mlirDenseElementsAttrUInt16Get shapeType' elements

denseElementsAttrUInt32 :: Type -> [Word32] -> DenseIntElementsAttr
denseElementsAttrUInt32 shapeType elements = Attribute $ \c -> do
  shapeType' <- getType shapeType c
  mlirDenseElementsAttrUInt32Get shapeType' elements

denseElementsAttrUInt64 :: Type -> [Word64] -> DenseIntElementsAttr
denseElementsAttrUInt64 shapeType elements = Attribute $ \c -> do
  shapeType' <- getType shapeType c
  mlirDenseElementsAttrUInt64Get shapeType' elements

-- TODO: Add more
type DenseFPElementsAttr = DenseElementsAttr
denseElementsAttrFloat :: Type -> [Float] -> DenseFPElementsAttr
denseElementsAttrFloat shapeType elements = Attribute $ \ c -> do 
  shapeType' <- getType shapeType c 
  mlirDenseElementsAttrFloatGet shapeType' elements


denseElementsAttrFloatSplat :: Type -> Float -> DenseFPElementsAttr 
denseElementsAttrFloatSplat shapeType e = Attribute $ \ c -> do
  shapeType' <- getType shapeType c 
  mlirDenseElementsAttrFloatSplatGet shapeType' e


-- TODO: To be implemented
type IntegerAttr = Attribute
type FloatAttr = Attribute
type UnitAttr = Attribute
type SymbolRefAttr = Attribute
type ElementsAttr = Attribute -- Doesn't make too many sense
type BoolAttr = Attribute
type AffineMapAttr = Attribute
type DictionaryAttr = Attribute
