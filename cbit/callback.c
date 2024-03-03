#include <mlir-c/Support.h>
#include "MLIR/FFI/Support_stub.h"
void mlirStringCallback(MlirStringRef string, void *userdata) {
    mlirStringCallback__cwrap((HsPtr)string.data, (HsWord64)string.length, userdata);
}
