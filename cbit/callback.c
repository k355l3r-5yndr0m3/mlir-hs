#include <HsFFI.h>
#include <mlir-c/Support.h>
#include "MLIR/FFI/Support_stub.h"

// extern void mlirStringCallback__cwrap(HsPtr a1, HsWord64 a2, HsPtr a3);
static void mlirStringCallback__hswrap__func(MlirStringRef string, void *userdata) {
    mlirStringCallback__cwrap((HsPtr)string.data, (HsWord64)string.length, userdata); 
}
MlirStringCallback mlirStringCallback__hswrap() { return mlirStringCallback__hswrap__func; }
