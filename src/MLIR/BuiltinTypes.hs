module MLIR.BuiltinTypes where
import MLIR.C.BuiltinTypes
import MLIR.IR (Type(..), Attribute(..))

import Foreign
import Foreign.C

integerType :: CUInt -> Type
integerType bitwidth = Type $ \c -> 
  mlirIntegerTypeGet c bitwidth

integerTypeSigned :: CUInt -> Type
integerTypeSigned bitwidth = Type $ \c -> 
  mlirIntegerTypeSignedGet c bitwidth

integerTypeUnsigned :: CUInt -> Type
integerTypeUnsigned bitwidth = Type $ \c ->
  mlirIntegerTypeUnsignedGet c bitwidth

indexType :: Type 
indexType = Type mlirIndexTypeGet

float8E5M2Type :: Type
float8E5M2Type = Type mlirFloat8E5M2TypeGet

float8E4M3FNType :: Type
float8E4M3FNType = Type mlirFloat8E4M3FNTypeGet

float8E5M2FNUZType :: Type
float8E5M2FNUZType = Type mlirFloat8E5M2FNUZTypeGet

float8E4M3B11FNUZType :: Type
float8E4M3B11FNUZType = Type mlirFloat8E4M3B11FNUZTypeGet


bf16Type :: Type
bf16Type = Type mlirBF16TypeGet

f16Type :: Type
f16Type = Type mlirF16TypeGet

f32Type :: Type
f32Type = Type mlirF32TypeGet

f64Type :: Type
f64Type = Type mlirF64TypeGet

noneType :: Type
noneType = Type mlirNoneTypeGet

complexType :: Type -> Type
complexType (Type tg) = Type $ \c -> 
  mlirComplexTypeGet =<< tg c

functionType :: [Type] -> [Type] -> Type
functionType inputs outputs = Type $ \c -> do 
  inputTypes  <- sequence $ fmap (\(Type tg) -> tg c) inputs
  outputTypes <- sequence $ fmap (\(Type tg) -> tg c) outputs
  mlirFunctionTypeGet c inputTypes outputTypes

rankedTensorType :: [Int64] -> Type -> Attribute -> Type
rankedTensorType shape (Type dtypeGet) (Attribute layoutGet) = Type $ \ctx -> do
  layout' <- layoutGet ctx 
  dtype' <- dtypeGet ctx
  mlirRankedTensorTypeGet shape dtype' layout'
