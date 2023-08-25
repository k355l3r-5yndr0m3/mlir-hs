#include <cstdint>
#include <mlir-c/IR.h>
#include <mlir/CAPI/IR.h>
#include <mlir/Dialect/GPU/IR/GPUDialect.h>

#define HS_ENUM_VALUE(type, hname, cname, value) extern "C" type hs__ ## hname ## __ ## value() { return (type) cname::value; }

#define HS_ENUM_GPU_DIMENSION(value) HS_ENUM_VALUE(uint32_t, mlirGpuDimension, mlir::gpu::Dimension, value)
static_assert(sizeof(mlir::gpu::Dimension) == sizeof(uint32_t), "mlir::gpu::Dimension bitwidth changed.");
HS_ENUM_GPU_DIMENSION(x);
HS_ENUM_GPU_DIMENSION(y);
HS_ENUM_GPU_DIMENSION(z);

extern "C" MlirAttribute hs__mlirGpuDimensionAttrGet(MlirContext context, uint32_t value) {
    return wrap(mlir::gpu::DimensionAttr::get(unwrap(context), (mlir::gpu::Dimension)value));
}

