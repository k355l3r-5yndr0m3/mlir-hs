module MLIR.FFI.BuiltinAttributes where

import MLIR.FFI.Marshal

import Data.Coerce
import Foreign


{#import MLIR.FFI.AffineExpr #}
{#import MLIR.FFI.Support #}
{#import MLIR.FFI.IR #}

#include <mlir-c/BuiltinAttributes.h>
#include <wrapper.h>
{#fun pure unsafe mlirAttributeIsAAffineMap { coerce `MlirAttribute' } -> `Bool' #}
{#fun pure unsafe mlirAffineMapAttrGet { coerce `MlirAffineMap' } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirAffineMapAttrGetValue { coerce `MlirAttribute' } -> `MlirAffineMap' coerce #}
{#fun pure unsafe mlirAffineMapAttrGetTypeID { } -> `MlirTypeID' coerce #}

{#fun pure unsafe mlirAttributeGetNull { } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirAttributeIsALocation { coerce `MlirAttribute' } -> `Bool' #}

{#fun pure unsafe mlirAttributeIsAArray { coerce `MlirAttribute' } -> `Bool' #}
{#fun pure unsafe mlirArrayAttrGet { coerce `MlirContext', marshalStorableArrayLen* `[MlirAttribute]'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirArrayAttrGetNumElements { coerce `MlirAttribute' } -> `Int' #}
{#fun pure unsafe mlirArrayAttrGetElement { coerce `MlirAttribute', `Int' } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirArrayAttrGetTypeID { } -> `MlirTypeID' coerce #}

{#fun pure unsafe mlirAttributeIsADictionary { coerce `MlirAttribute' } -> `Bool' #}
{#fun pure unsafe mlirDictionaryAttrGet { coerce `MlirContext', marshalStorableArrayLen* `[MlirNamedAttribute]'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDictionaryAttrGetNumElements { coerce `MlirAttribute' } -> `Int' #}
{#fun pure unsafe mlirDictionaryAttrGetElement__hswrap as mlirDictionaryAttrGetElement { coerce `MlirAttribute', `Int', alloca- `MlirNamedAttribute' peek*} -> `()' #}
{#fun pure unsafe mlirDictionaryAttrGetElementByName__hswrap as mlirDictionaryAttrGetElementByName { coerce `MlirAttribute', `String'& } -> `MlirAttribute' coerce #} 
{#fun pure unsafe mlirDictionaryAttrGetTypeID { } -> `MlirTypeID' coerce #}

{#fun pure unsafe mlirAttributeIsAFloat { coerce `MlirAttribute' } -> `Bool' #}
{#fun pure unsafe mlirFloatAttrDoubleGet { coerce `MlirContext', coerce `MlirType', `Double' } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirFloatAttrDoubleGetChecked { coerce `MlirLocation', coerce `MlirType', `Double' } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirFloatAttrGetValueDouble { coerce `MlirAttribute' } -> `Double' #}
{#fun pure unsafe mlirFloatAttrGetTypeID { } -> `MlirTypeID' coerce #}

{#fun pure unsafe mlirAttributeIsAInteger { coerce `MlirAttribute' } -> `Bool' #}
{#fun pure unsafe mlirIntegerAttrGet { coerce `MlirType', `Int64' } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirIntegerAttrGetValueInt { coerce `MlirAttribute' } -> `Int64' #}
{#fun pure unsafe mlirIntegerAttrGetValueSInt { coerce `MlirAttribute' } -> `Int64' #}
{#fun pure unsafe mlirIntegerAttrGetValueUInt { coerce `MlirAttribute' } -> `Word64' #}
{#fun pure unsafe mlirIntegerAttrGetTypeID { } -> `MlirTypeID' coerce #}

{#fun pure unsafe mlirAttributeIsABool { coerce `MlirAttribute' } -> `Bool' #}
{#fun pure unsafe mlirBoolAttrGet { coerce `MlirContext', `Bool' } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirBoolAttrGetValue { coerce `MlirAttribute' } -> `Bool' #}

{#fun pure unsafe mlirAttributeIsAIntegerSet { coerce `MlirAttribute' } -> `Bool' #}
{#fun pure unsafe mlirIntegerSetAttrGetTypeID { } -> `MlirTypeID' coerce #}

{#fun pure unsafe mlirAttributeIsAOpaque { coerce `MlirAttribute' } -> `Bool' #}
{#fun pure unsafe mlirOpaqueAttrGet__hswrap { coerce `MlirContext', `String'&, `Int', castPtr `Ptr Word8', coerce `MlirType' } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirOpaqueAttrGetDialectNamespace__hswrap { coerce `MlirAttribute', alloca- `String' peekStringRefPtr* } -> `()' #}
{#fun pure unsafe mlirOpaqueAttrGetData__hswrap { coerce `MlirAttribute', alloca- `MlirStringRef' peek* } -> `()' #}
{#fun pure unsafe mlirOpaqueAttrGetTypeID { } -> `MlirTypeID' coerce #}

{#fun pure unsafe mlirAttributeIsAString { coerce `MlirAttribute' } -> `Bool' #}
{#fun pure unsafe mlirStringAttrGet__hswrap as mlirStringAttrGet { coerce `MlirContext', `String'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirStringAttrTypedGet__hswrap as mlirStringAttrTypedGet { coerce `MlirType', `String'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirStringAttrGetValue__hswrap as mlirStringAttrGetValue { coerce `MlirAttribute', alloca- `String' peekStringRefPtr* } -> `()' #}
{#fun pure unsafe mlirStringAttrGetTypeID { } -> `MlirTypeID' coerce #}

-- TODO: Add SymbolRef attribute.
-- TODO: Add Flat SymbolRef attribute

{#fun pure unsafe mlirAttributeIsAType { coerce `MlirAttribute' } -> `Bool' #}
{#fun pure unsafe mlirTypeAttrGet { coerce `MlirType' } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirTypeAttrGetValue { coerce `MlirAttribute' } -> `MlirType' coerce #}
{#fun pure unsafe mlirTypeAttrGetTypeID { } -> `MlirTypeID' coerce #}

{#fun pure unsafe mlirAttributeIsAUnit { coerce `MlirAttribute' } -> `Bool' #}
{#fun pure unsafe mlirUnitAttrGet { coerce `MlirContext' } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirUnitAttrGetTypeID { } -> `MlirTypeID' coerce #}

{#fun pure unsafe mlirAttributeIsAElements { coerce `MlirAttribute' } -> `Bool' #}
{#fun pure unsafe mlirElementsAttrGetValue { coerce `MlirAttribute', marshalIntegralArrayLen* `[Int64]'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirElementsAttrIsValidIndex { coerce `MlirAttribute', marshalIntegralArrayLen* `[Int64]'& } -> `Bool' #}
{#fun pure unsafe mlirElementsAttrGetNumElements { coerce `MlirAttribute' } -> `Int64' #}

{#fun pure unsafe mlirDenseArrayAttrGetTypeID { } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirAttributeIsADenseBoolArray { coerce `MlirAttribute' } -> `Bool' #}
{#fun pure unsafe mlirAttributeIsADenseI8Array { coerce `MlirAttribute' } -> `Bool' #}
{#fun pure unsafe mlirAttributeIsADenseI16Array { coerce `MlirAttribute' } -> `Bool' #}
{#fun pure unsafe mlirAttributeIsADenseI32Array { coerce `MlirAttribute' } -> `Bool' #}
{#fun pure unsafe mlirAttributeIsADenseI64Array { coerce `MlirAttribute' } -> `Bool' #}
{#fun pure unsafe mlirAttributeIsADenseF32Array { coerce `MlirAttribute' } -> `Bool' #}
{#fun pure unsafe mlirAttributeIsADenseF64Array { coerce `MlirAttribute' } -> `Bool' #}

{#fun pure unsafe mlirDenseBoolArrayGet { coerce `MlirContext', marshalBoolArrayLen* `[Bool]'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseI8ArrayGet { coerce `MlirContext', marshalPrimArray* `PrimArray Int8'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseI16ArrayGet { coerce `MlirContext', marshalPrimArray* `PrimArray Int16'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseI32ArrayGet { coerce `MlirContext', marshalPrimArray* `PrimArray Int32'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseI64ArrayGet { coerce `MlirContext', marshalPrimArray* `PrimArray Int64'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseF32ArrayGet { coerce `MlirContext', marshalPrimArray* `PrimArray Float'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseF64ArrayGet { coerce `MlirContext', marshalPrimArray* `PrimArray Double'& } -> `MlirAttribute' coerce #}

{#fun pure unsafe mlirDenseArrayGetNumElements { coerce `MlirAttribute' } -> `Int' #}
{#fun pure unsafe mlirDenseBoolArrayGetElement { coerce `MlirAttribute', `Int' } -> `Bool' #}
{#fun pure unsafe mlirDenseI8ArrayGetElement { coerce `MlirAttribute', `Int' } -> `Int8' #}
{#fun pure unsafe mlirDenseI16ArrayGetElement { coerce `MlirAttribute', `Int' } -> `Int16' #}
{#fun pure unsafe mlirDenseI32ArrayGetElement { coerce `MlirAttribute', `Int' } -> `Int32' #}
{#fun pure unsafe mlirDenseI64ArrayGetElement { coerce `MlirAttribute', `Int' } -> `Int64' #}
{#fun pure unsafe mlirDenseF32ArrayGetElement { coerce `MlirAttribute', `Int' } -> `Float' #}
{#fun pure unsafe mlirDenseF64ArrayGetElement { coerce `MlirAttribute', `Int' } -> `Double' #}

{#fun pure unsafe mlirAttributeIsADenseElements { coerce `MlirAttribute' } -> `Bool' #}
{#fun pure unsafe mlirAttributeIsADenseIntElements { coerce `MlirAttribute' } -> `Bool' #}
{#fun pure unsafe mlirAttributeIsADenseFPElements { coerce `MlirAttribute' } -> `Bool' #}

{#fun pure unsafe mlirDenseIntOrFPElementsAttrGetTypeID { } -> `MlirTypeID' coerce #}

{#fun pure unsafe mlirDenseElementsAttrGet { coerce `MlirType', marshalStorableArrayLen* `[MlirAttribute]'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseElementsAttrRawBufferGet { coerce `MlirType', `Int', castPtr `Ptr Word8' } -> `MlirAttribute' coerce #} 
{#fun pure unsafe mlirDenseElementsAttrSplatGet { coerce `MlirType', coerce `MlirAttribute' } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseElementsAttrBoolSplatGet { coerce `MlirType', `Bool' } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseElementsAttrUInt8SplatGet { coerce `MlirType', `Word8' } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseElementsAttrInt8SplatGet { coerce `MlirType', `Int8' } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseElementsAttrUInt32SplatGet { coerce `MlirType', `Word32' } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseElementsAttrInt32SplatGet { coerce `MlirType', `Int32' } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseElementsAttrUInt64SplatGet { coerce `MlirType', `Word64' } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseElementsAttrInt64SplatGet { coerce `MlirType', `Int64' } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseElementsAttrFloatSplatGet { coerce `MlirType', `Float' } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseElementsAttrDoubleSplatGet { coerce `MlirType', `Double' } -> `MlirAttribute' coerce #}

-- TODO: Change to bytearray
-- {#fun pure unsafe mlirDenseElementsAttrBoolGet { coerce `MlirType', marshalPrimArray* `PrimArray Int32'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseElementsAttrUInt8Get { coerce `MlirType', marshalPrimArray* `PrimArray Word8'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseElementsAttrInt8Get { coerce `MlirType', marshalPrimArray* `PrimArray Int8'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseElementsAttrUInt16Get { coerce `MlirType', marshalPrimArray* `PrimArray Word16'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseElementsAttrInt16Get { coerce `MlirType', marshalPrimArray* `PrimArray Int16'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseElementsAttrUInt32Get { coerce `MlirType', marshalPrimArray* `PrimArray Word32'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseElementsAttrInt32Get { coerce `MlirType', marshalPrimArray* `PrimArray Int32'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseElementsAttrUInt64Get { coerce `MlirType', marshalPrimArray* `PrimArray Word64'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseElementsAttrInt64Get { coerce `MlirType', marshalPrimArray* `PrimArray Int64'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseElementsAttrFloatGet { coerce `MlirType', marshalPrimArray* `PrimArray Float'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseElementsAttrDoubleGet { coerce `MlirType', marshalPrimArray* `PrimArray Double'& } -> `MlirAttribute' coerce #}
-- {#fun pure unsafe mlirDenseElementsAttrBFloat16Get { coerce `MlirType',   } -> `MlirAttribute' coerce #}
-- {#fun pure unsafe mlirDenseElementsAttrFloat16Get { coerce `MlirType',   } -> `MlirAttribute' coerce #}
-- mlirDenseElementsAttrStringGet

{#fun pure unsafe mlirDenseElementsAttrReshapeGet { coerce `MlirAttribute', coerce `MlirType' } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseElementsAttrIsSplat { coerce `MlirAttribute' } -> `Bool' #}

{#fun pure unsafe mlirDenseElementsAttrGetSplatValue { coerce `MlirAttribute' } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirDenseElementsAttrGetBoolSplatValue { coerce `MlirAttribute' } -> `Bool' #}
{#fun pure unsafe mlirDenseElementsAttrGetInt8SplatValue { coerce `MlirAttribute' } -> `Int8' #}
{#fun pure unsafe mlirDenseElementsAttrGetUInt8SplatValue { coerce `MlirAttribute' } -> `Word8' #}
{#fun pure unsafe mlirDenseElementsAttrGetInt32SplatValue { coerce `MlirAttribute' } -> `Int32' #}
{#fun pure unsafe mlirDenseElementsAttrGetUInt32SplatValue { coerce `MlirAttribute' } -> `Word32' #}
{#fun pure unsafe mlirDenseElementsAttrGetInt64SplatValue { coerce `MlirAttribute' } -> `Int64' #}
{#fun pure unsafe mlirDenseElementsAttrGetUInt64SplatValue { coerce `MlirAttribute' } -> `Word64' #}
{#fun pure unsafe mlirDenseElementsAttrGetFloatSplatValue { coerce `MlirAttribute' } -> `Float' #}
{#fun pure unsafe mlirDenseElementsAttrGetDoubleSplatValue { coerce `MlirAttribute' } -> `Double' #}

-- mlirDenseElementsAttrGetStringSplatValue

{#fun pure unsafe mlirDenseElementsAttrGetBoolValue { coerce `MlirAttribute', `Int' } -> `Bool' #} 
{#fun pure unsafe mlirDenseElementsAttrGetInt8Value { coerce `MlirAttribute', `Int' } -> `Int8' coerce #}
{#fun pure unsafe mlirDenseElementsAttrGetUInt8Value { coerce `MlirAttribute', `Int' } -> `Word8' coerce #}
{#fun pure unsafe mlirDenseElementsAttrGetInt16Value { coerce `MlirAttribute', `Int' } -> `Int16' coerce #}
{#fun pure unsafe mlirDenseElementsAttrGetUInt16Value { coerce `MlirAttribute', `Int' } -> `Word16' coerce #}
{#fun pure unsafe mlirDenseElementsAttrGetInt32Value { coerce `MlirAttribute', `Int' } -> `Int32' coerce #}
{#fun pure unsafe mlirDenseElementsAttrGetUInt32Value { coerce `MlirAttribute', `Int' } -> `Word32' coerce #}
{#fun pure unsafe mlirDenseElementsAttrGetInt64Value { coerce `MlirAttribute', `Int' } -> `Int64' coerce #}
{#fun pure unsafe mlirDenseElementsAttrGetUInt64Value { coerce `MlirAttribute', `Int' } -> `Word64' coerce #}
{#fun pure unsafe mlirDenseElementsAttrGetFloatValue { coerce `MlirAttribute', `Int' } -> `Float' coerce #}
{#fun pure unsafe mlirDenseElementsAttrGetDoubleValue { coerce `MlirAttribute', `Int' } -> `Double' coerce #}

-- mlirDenseElementsAttrGetStringValue
-- mlirDenseElementsAttrGetRawData

-- TODO: Resource blob attributes.
-- TODO: Sparse elements attribute.
{#fun pure unsafe mlirAttributeIsAStridedLayout { coerce `MlirAttribute' } -> `Bool' #}
{#fun pure unsafe mlirStridedLayoutAttrGet { coerce `MlirContext', coerce `Int64', marshalIntegralArrayLen* `[Int64]'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirStridedLayoutAttrGetOffset { coerce `MlirAttribute' } -> `Int64' coerce #}
{#fun pure unsafe mlirStridedLayoutAttrGetNumStrides { coerce `MlirAttribute' } -> `Int' #}
{#fun pure unsafe mlirStridedLayoutAttrGetStride { coerce `MlirAttribute', `Int' } -> `Int64' coerce #}
{#fun pure unsafe mlirStridedLayoutAttrGetTypeID { } -> `MlirTypeID' coerce #}

