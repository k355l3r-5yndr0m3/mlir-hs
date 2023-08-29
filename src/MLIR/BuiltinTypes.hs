{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module MLIR.BuiltinTypes where
import qualified MLIR.C.BuiltinTypes as C
-- import qualified MLIR.C.IR as C
import MLIR.IR

import Foreign
import Foreign.C


-- Integer
data SignednessSemantics = Unsigned | Signed | Signless
data IntegerType = IntegerType SignednessSemantics CUInt
pattern SI :: CUInt -> IntegerType
pattern SI w = IntegerType Signed w
pattern UI :: CUInt -> IntegerType
pattern UI w = IntegerType Unsigned w
pattern I :: CUInt -> IntegerType
pattern I w = IntegerType Signless w

pattern SI8 :: IntegerType
pattern SI16 :: IntegerType
pattern SI32 :: IntegerType
pattern SI64 :: IntegerType
pattern SI8 = SI 8
pattern SI16 = SI 16
pattern SI32 = SI 32
pattern SI64 = SI 64

pattern UI8 :: IntegerType
pattern UI16 :: IntegerType
pattern UI32 :: IntegerType
pattern UI64 :: IntegerType
pattern UI8 = UI 8
pattern UI16 = UI 16
pattern UI32 = UI 32
pattern UI64 = UI 64

pattern I8 :: IntegerType
pattern I16 :: IntegerType
pattern I32 :: IntegerType
pattern I64 :: IntegerType
pattern I8 = I 8
pattern I16 = I 16
pattern I32 = I 32
pattern I64 = I 64

instance TypeGet IntegerType where
  typeGet (IntegerType s w) c = intTypeGet c w
    where intTypeGet = 
            case s of 
              Unsigned -> C.integerTypeUnsignedGet 
              Signed   -> C.integerTypeSignedGet
              Signless -> C.integerTypeGet

-- Index 
data Index
instance TypeGet Index where
  typeGet _ = C.indexTypeGet

-- Floating
data Float8E5M2Type = Float8E5M2Type
data Float8E4M3FNType = Float8E4M3FNType
data Float8E5M2FNUZType = Float8E5M2FNUZType
data Float8E4M3FNUZType = Float8E4M3FNUZType
data Float8E4M3B11FNUZType = Float8E4M3B11FNUZType
data BF16Type = BF16Type
data F16Type = F16Type
data F32Type = F32Type
data F64Type = F64Type
data TF32Type = TF32Type
instance TypeGet Float8E5M2Type where
  typeGet _ = C.float8E5M2TypeGet
instance TypeGet Float8E4M3FNType where
  typeGet _ = C.float8E4M3FNTypeGet
instance TypeGet Float8E5M2FNUZType where
  typeGet _ = C.float8E5M2FNUZTypeGet
instance TypeGet Float8E4M3FNUZType where
  typeGet _ = C.float8E4M3FNUZTypeGet
instance TypeGet Float8E4M3B11FNUZType where 
  typeGet _ = C.float8E4M3B11FNUZTypeGet
instance TypeGet BF16Type where
  typeGet _ = C.bf16TypeGet
instance TypeGet F16Type where
  typeGet _ = C.f16TypeGet
instance TypeGet F32Type where
  typeGet _ = C.f32TypeGet
instance TypeGet F64Type where
  typeGet _ = C.f64TypeGet
instance TypeGet TF32Type where
  typeGet _ = C.tf32TypeGet


-- Complex
newtype ComplexType a = ComplexType a
instance TypeGet a => TypeGet (ComplexType a) where
  typeGet (ComplexType t) c = C.complexTypeGet =<< typeGet t c

-- ShapedType
data VectorType a = VectorType [Int64] a
instance TypeGet a => TypeGet (VectorType a) where
  typeGet (VectorType shape t) c = 
    withArrayLen shape $ \ (fromIntegral -> rank) shape' -> 
      C.vectorTypeGet rank shape' =<< typeGet t c

data RankedTensorType e t = RankedTensorType [Int64] t e
instance (TypeGet t, AttrGet e) => TypeGet (RankedTensorType e t) where
  typeGet (RankedTensorType shape elemType encoding) c = 
    withArrayLen shape $ \ (fromIntegral -> rank) shape' -> do
      el <- typeGet elemType c
      en <- attrGet encoding c
      C.rankedTensorTypeGet rank shape' el en

newtype UnrankedTensorType a = UnrankedTensorType a
instance TypeGet a => TypeGet (UnrankedTensorType a) where
  typeGet (UnrankedTensorType a) c = C.unrankedTensorTypeGet =<< typeGet a c

data FunctionType = FunctionType [AnyType] [AnyType]
instance TypeGet FunctionType where
  typeGet (FunctionType inputs outputs) c = do
    inputs'  <- mapM (`typeGet` c) inputs 
    outputs' <- mapM (`typeGet` c) outputs
    withArrayLen inputs' $ \ (fromIntegral -> numInputs) ins -> 
      withArrayLen outputs' $ \ (fromIntegral -> numOutputs) outs -> 
        C.functionTypeGet c numInputs ins numOutputs outs

class TypeGet a => ShapedType a
instance TypeGet a => ShapedType (VectorType a)
instance (TypeGet t, AttrGet e) => ShapedType (RankedTensorType e t) -- TODO: Fignure out what is encoding
instance TypeGet a => ShapedType (UnrankedTensorType a)

