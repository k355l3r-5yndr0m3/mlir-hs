module MLIR.Dialect.LLVM.Extra
( mlirLLVMPointerTypeGet  
, mlirLLVMVoidTypeGet     
, mlirLLVMArrayTypeGet    
, mlirLLVMFunctionTypeGet 
, mlirLLVMStructTypeLiteralGet
)where
{#import MLIR.FFI.IR #}
import MLIR.FFI.Marshal
import Data.Coerce

#include <mlir-c/Dialect/LLVM.h>

{#fun pure unsafe mlirLLVMPointerTypeGet       { coerce `MlirType', fromIntegral `Word' } -> `MlirType' coerce #}
{#fun pure unsafe mlirLLVMVoidTypeGet          { coerce `MlirContext' } -> `MlirType' coerce #}
{#fun pure unsafe mlirLLVMArrayTypeGet         { coerce `MlirType', fromIntegral `Word' } -> `MlirType' coerce #}
{#fun pure unsafe mlirLLVMFunctionTypeGet      { coerce `MlirType', marshalStorableArrayLen* `[MlirType]'&, `Bool' } -> `MlirType' coerce #}
{#fun pure unsafe mlirLLVMStructTypeLiteralGet { coerce `MlirContext', marshalStorableArrayLen* `[MlirType]'&, `Bool' } -> `MlirType' coerce #}
