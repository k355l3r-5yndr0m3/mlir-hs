{-# OPTIONS_GHC -W #-}
module MLIR.FFI.BuiltinTypes
( mlirIntegerTypeGetTypeID
, mlirTypeIsAInteger
, mlirIntegerTypeGet
, mlirIntegerTypeSignedGet
, mlirIntegerTypeUnsignedGet
, mlirIntegerTypeGetWidth
, mlirIntegerTypeIsSignless
, mlirIntegerTypeIsSigned
, mlirIntegerTypeIsUnsigned
, mlirIndexTypeGetTypeID
, mlirTypeIsAIndex
, mlirIndexTypeGet
, mlirFloat8E5M2TypeGetTypeID
, mlirTypeIsAFloat8E5M2
, mlirFloat8E5M2TypeGet
, mlirFloat8E4M3FNTypeGetTypeID
, mlirTypeIsAFloat8E4M3FN
, mlirFloat8E4M3FNTypeGet
, mlirFloat8E5M2FNUZTypeGetTypeID
, mlirTypeIsAFloat8E5M2FNUZ
, mlirFloat8E5M2FNUZTypeGet
, mlirFloat8E4M3FNUZTypeGetTypeID
, mlirTypeIsAFloat8E4M3FNUZ
, mlirFloat8E4M3FNUZTypeGet
, mlirFloat8E4M3B11FNUZTypeGetTypeID
, mlirTypeIsAFloat8E4M3B11FNUZ
, mlirFloat8E4M3B11FNUZTypeGet
, mlirBFloat16TypeGetTypeID
, mlirTypeIsABF16
, mlirBF16TypeGet
, mlirFloat16TypeGetTypeID
, mlirTypeIsAF16
, mlirF16TypeGet
, mlirFloat32TypeGetTypeID
, mlirTypeIsAF32
, mlirF32TypeGet
, mlirFloat64TypeGetTypeID
, mlirTypeIsAF64
, mlirF64TypeGet
, mlirFloatTF32TypeGetTypeID
, mlirTypeIsATF32
, mlirTF32TypeGet
, mlirNoneTypeGetTypeID
, mlirTypeIsANone
, mlirNoneTypeGet
, mlirComplexTypeGetTypeID
, mlirTypeIsAComplex
, mlirComplexTypeGet
, mlirComplexTypeGetElementType
, mlirTypeIsAShaped
, mlirShapedTypeGetElementType
, mlirShapedTypeHasRank
, mlirShapedTypeGetRank
, mlirShapedTypeHasStaticShape
, mlirShapedTypeIsDynamicDim
, mlirShapedTypeGetDimSize
, mlirShapedTypeIsDynamicSize
, mlirShapedTypeIsDynamicStrideOrOffset
, mlirVectorTypeGetTypeID
, mlirTypeIsAVector
, mlirVectorTypeGet
, mlirVectorTypeGetChecked
, mlirTypeIsATensor
, mlirRankedTensorTypeGetTypeID
, mlirTypeIsARankedTensor
, mlirUnrankedTensorTypeGetTypeID
, mlirTypeIsAUnrankedTensor
, mlirRankedTensorTypeGet
, mlirRankedTensorTypeGetChecked
, mlirRankedTensorTypeGetEncoding
, mlirUnrankedTensorTypeGet
, mlirUnrankedTensorTypeGetChecked
, mlirMemRefTypeGetTypeID
, mlirTypeIsAMemRef
, mlirUnrankedMemRefTypeGetTypeID
, mlirTypeIsAUnrankedMemRef
, mlirMemRefTypeGet
, mlirMemRefTypeGetChecked
, mlirMemRefTypeContiguousGet
, mlirMemRefTypeContiguousGetChecked
, mlirUnrankedMemRefTypeGet
, mlirUnrankedMemRefTypeGetChecked
, mlirMemRefTypeGetLayout
, mlirMemRefTypeGetMemorySpace
, mlirUnrankedMemrefGetMemorySpace
, mlirTupleTypeGetTypeID
, mlirTypeIsATuple
, mlirTupleTypeGet
, mlirTupleTypeGetNumTypes
, mlirTupleTypeGetType
, mlirFunctionTypeGetTypeID
, mlirTypeIsAFunction
, mlirFunctionTypeGet
, mlirFunctionTypeGetNumInputs
, mlirFunctionTypeGetNumResults
, mlirFunctionTypeGetInput
, mlirFunctionTypeGetResult
, mlirOpaqueTypeGetTypeID
, mlirOpaqueTypeGetData
, mlirTypeIsAOpaque
, mlirOpaqueTypeGet
)where
import MLIR.FFI.Marshal
import Foreign

import Data.Coerce

{#import MLIR.FFI.Support #}
{#import MLIR.FFI.IR #}

#include <mlir-c/BuiltinTypes.h>
#include <wrapper.h>

{#fun pure unsafe mlirIntegerTypeGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirTypeIsAInteger { coerce `MlirType' } -> `Bool' #}

{#fun pure unsafe mlirIntegerTypeGet         { coerce `MlirContext', fromIntegral `Word' } -> `MlirType' coerce #}
{#fun pure unsafe mlirIntegerTypeSignedGet   { coerce `MlirContext', fromIntegral `Word' } -> `MlirType' coerce #}
{#fun pure unsafe mlirIntegerTypeUnsignedGet { coerce `MlirContext', fromIntegral `Word' } -> `MlirType' coerce #}

{#fun pure unsafe mlirIntegerTypeGetWidth { coerce `MlirType' } -> `Word' fromIntegral #}

{#fun pure unsafe mlirIntegerTypeIsSignless { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirIntegerTypeIsSigned   { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirIntegerTypeIsUnsigned { coerce `MlirType' } -> `Bool' #}

{#fun pure unsafe mlirIndexTypeGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirTypeIsAIndex { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirIndexTypeGet { coerce `MlirContext' } -> `MlirType' coerce #}

{#fun pure unsafe mlirFloat8E5M2TypeGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirTypeIsAFloat8E5M2 { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirFloat8E5M2TypeGet { coerce `MlirContext' } -> `MlirType' coerce #}

{#fun pure unsafe mlirFloat8E4M3FNTypeGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirTypeIsAFloat8E4M3FN { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirFloat8E4M3FNTypeGet { coerce `MlirContext' } -> `MlirType' coerce #}

{#fun pure unsafe mlirFloat8E5M2FNUZTypeGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirTypeIsAFloat8E5M2FNUZ { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirFloat8E5M2FNUZTypeGet { coerce `MlirContext' } -> `MlirType' coerce #}

{#fun pure unsafe mlirFloat8E4M3FNUZTypeGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirTypeIsAFloat8E4M3FNUZ { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirFloat8E4M3FNUZTypeGet { coerce `MlirContext' } -> `MlirType' coerce #}

{#fun pure unsafe mlirFloat8E4M3B11FNUZTypeGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirTypeIsAFloat8E4M3B11FNUZ { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirFloat8E4M3B11FNUZTypeGet { coerce `MlirContext' } -> `MlirType' coerce #}

{#fun pure unsafe mlirBFloat16TypeGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirTypeIsABF16 { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirBF16TypeGet { coerce `MlirContext' } -> `MlirType' coerce #}

{#fun pure unsafe mlirFloat16TypeGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirTypeIsAF16 { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirF16TypeGet { coerce `MlirContext' } -> `MlirType' coerce #}

{#fun pure unsafe mlirFloat32TypeGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirTypeIsAF32 { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirF32TypeGet { coerce `MlirContext' } -> `MlirType' coerce #}

{#fun pure unsafe mlirFloat64TypeGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirTypeIsAF64 { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirF64TypeGet { coerce `MlirContext' } -> `MlirType' coerce #}

{#fun pure unsafe mlirFloatTF32TypeGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirTypeIsATF32 { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirTF32TypeGet { coerce `MlirContext' } -> `MlirType' coerce #}

{#fun pure unsafe mlirNoneTypeGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirTypeIsANone { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirNoneTypeGet { coerce `MlirContext' } -> `MlirType' coerce #}

{#fun pure unsafe mlirComplexTypeGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirTypeIsAComplex { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirComplexTypeGet { coerce `MlirType' } -> `MlirType' coerce #}
{#fun pure unsafe mlirComplexTypeGetElementType { coerce `MlirType' } -> `MlirType' coerce #}

{#fun pure unsafe mlirTypeIsAShaped { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirShapedTypeGetElementType { coerce `MlirType' } -> `MlirType' coerce #}
{#fun pure unsafe mlirShapedTypeHasRank { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirShapedTypeGetRank { coerce `MlirType' } -> `Int' #}
{#fun pure unsafe mlirShapedTypeHasStaticShape { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirShapedTypeIsDynamicDim { coerce `MlirType', `Int' } -> `Bool' #}
{#fun pure unsafe mlirShapedTypeGetDimSize { coerce `MlirType', `Int' } -> `Int64' #}
{#fun pure unsafe mlirShapedTypeIsDynamicSize { `Int64' } -> `Bool' #}
{#fun pure unsafe mlirShapedTypeIsDynamicStrideOrOffset { `Int64' } -> `Bool' #}

{#fun pure unsafe mlirVectorTypeGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirTypeIsAVector { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirVectorTypeGet { marshalIntegralArrayLen* `[Int64]'&, coerce `MlirType' } -> `MlirType' coerce #}
{#fun pure unsafe mlirVectorTypeGetChecked { coerce `MlirLocation', marshalIntegralArrayLen* `[Int64]'&, coerce `MlirType' } -> `MlirType' coerce #}

{#fun pure unsafe mlirTypeIsATensor { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirRankedTensorTypeGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirTypeIsARankedTensor { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirUnrankedTensorTypeGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirTypeIsAUnrankedTensor { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirRankedTensorTypeGet { marshalIntegralArrayLen* `[Int64]'&, coerce `MlirType', coerce `MlirAttribute' } -> `MlirType' coerce #}
{#fun pure unsafe mlirRankedTensorTypeGetChecked { coerce `MlirLocation', marshalIntegralArrayLen* `[Int64]'&, coerce `MlirType', coerce `MlirAttribute' } -> `MlirType' coerce #}
{#fun pure unsafe mlirRankedTensorTypeGetEncoding { coerce `MlirType' } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirUnrankedTensorTypeGet { coerce `MlirType' } -> `MlirType' coerce #}
{#fun pure unsafe mlirUnrankedTensorTypeGetChecked { coerce `MlirLocation', coerce `MlirType' } -> `MlirType' coerce #}

{#fun pure unsafe mlirMemRefTypeGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirTypeIsAMemRef { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirUnrankedMemRefTypeGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirTypeIsAUnrankedMemRef { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirMemRefTypeGet { coerce `MlirType', marshalIntegralArrayLen* `[Int64]'&, coerce `MlirAttribute', coerce `MlirAttribute' } -> `MlirType' coerce #}
{#fun pure unsafe mlirMemRefTypeGetChecked { coerce `MlirLocation', coerce `MlirType', marshalIntegralArrayLen* `[Int64]'&, coerce `MlirAttribute', coerce `MlirAttribute' } -> `MlirType' coerce #}
{#fun pure unsafe mlirMemRefTypeContiguousGet { coerce `MlirType', marshalIntegralArrayLen* `[Int64]'&, coerce `MlirAttribute' } -> `MlirType' coerce #}
{#fun pure unsafe mlirMemRefTypeContiguousGetChecked { coerce `MlirLocation', coerce `MlirType', marshalIntegralArrayLen* `[Int64]'&, coerce `MlirAttribute' } -> `MlirType' coerce #}
{#fun pure unsafe mlirUnrankedMemRefTypeGet { coerce `MlirType', coerce `MlirAttribute' } -> `MlirType' coerce #}
{#fun pure unsafe mlirUnrankedMemRefTypeGetChecked { coerce `MlirLocation', coerce `MlirType', coerce `MlirAttribute' } -> `MlirType' coerce #}

{#fun pure unsafe mlirMemRefTypeGetLayout { coerce `MlirType' } -> `MlirAttribute' coerce #}
-- mlirMemRefTypeGetAffineMap 
{#fun pure unsafe mlirMemRefTypeGetMemorySpace { coerce `MlirType' } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirUnrankedMemrefGetMemorySpace { coerce `MlirType' } -> `MlirAttribute' coerce #}

{#fun pure unsafe mlirTupleTypeGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirTypeIsATuple { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirTupleTypeGet { coerce `MlirContext', marshalStorableArrayLen* `[MlirType]'& } -> `MlirType' coerce #}
{#fun pure unsafe mlirTupleTypeGetNumTypes { coerce `MlirType' } -> `Int' #}
{#fun pure unsafe mlirTupleTypeGetType { coerce `MlirType', `Int' } -> `MlirType' coerce #}

{#fun pure unsafe mlirFunctionTypeGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirTypeIsAFunction { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirFunctionTypeGet { coerce `MlirContext', marshalStorableArrayLen* `[MlirType]'&, marshalStorableArrayLen* `[MlirType]'& } -> `MlirType' coerce #}
{#fun pure unsafe mlirFunctionTypeGetNumInputs { coerce `MlirType' } -> `Int' #}
{#fun pure unsafe mlirFunctionTypeGetNumResults { coerce `MlirType' } -> `Int' #}

{#fun pure unsafe mlirFunctionTypeGetInput { coerce `MlirType', `Int' } -> `MlirType' coerce #}
{#fun pure unsafe mlirFunctionTypeGetResult { coerce `MlirType', `Int' } -> `MlirType' coerce #}

{#fun pure unsafe mlirOpaqueTypeGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirTypeIsAOpaque { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirOpaqueTypeGet__hswrap as mlirOpaqueTypeGet { coerce `MlirContext', `String'&, `String'& } -> `MlirType' coerce #}
{#fun pure unsafe mlirOpaqueTypeGetData__hswrap as mlirOpaqueTypeGetData { coerce `MlirType', alloca- `String' peekStringRefPtr*} -> `()' #}
