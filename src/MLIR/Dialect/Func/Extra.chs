module MLIR.Dialect.Func.Extra
( mlirFuncSetArgAttr
) where
{#import MLIR.FFI.IR #}
import Data.Coerce
#include <mlir-c/Dialect/Func.h>

#c
void mlirFuncSetArgAttr__hswrap(MlirOperation op, intptr_t pos, const char *name_data, size_t name_length, MlirAttribute attr) {
    mlirFuncSetArgAttr(op, pos, (MlirStringRef){name_data, name_length}, attr);
}
#endc
{#fun unsafe mlirFuncSetArgAttr__hswrap as mlirFuncSetArgAttr { coerce `MlirOperation', `Int', `String'&, coerce `MlirAttribute' } -> `()' #}
