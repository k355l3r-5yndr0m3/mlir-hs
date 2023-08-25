#include <stddef.h>

#include <mlir-c/Support.h>
#include <mlir-c/IR.h>
#include <mlir-c/BuiltinAttributes.h>

MlirAttribute hs__mlirStringAttrGet(MlirContext ctx, const char *str, size_t strlength) {
    return mlirStringAttrGet(ctx, (MlirStringRef){.data = str, .length = strlength});
}

MlirAttribute hs__mlirFlatSymbolRefAttrGet(MlirContext context, const char *data, size_t length) {
    return mlirFlatSymbolRefAttrGet(context, (MlirStringRef){.data = data, .length = length});
}

MlirAttribute hs__mlirStringAttrTypedGet(MlirType type, const char *data, size_t length) {
    return mlirStringAttrTypedGet(type, (MlirStringRef){.data = data, .length = length});
}
