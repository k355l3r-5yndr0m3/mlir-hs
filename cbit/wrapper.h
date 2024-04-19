#pragma once

#include <mlir-c/Support.h>
#include <mlir-c/IR.h>
#include <mlir-c/BuiltinTypes.h>

#include <stddef.h>
#include <stdint.h>

extern void *mlirContextGetOrLoadDialect__hswrap(void *ctx, const char *name_data, size_t name_length);
extern bool mlirContextIsRegisteredOperation__hswrap(void *cxt, const char *name_data, size_t name_length);

extern void mlirDialectGetNamespace__hswrap(void *dialect, MlirStringRef *output);
extern void mlirDialectHandleGetNamespace__hswrap(void *dialectHandle, MlirStringRef *output);

extern const void *mlirModuleCreateParse__hswrap(void *ctx, const char *module_data, size_t module_length);

extern void mlirOperationStateGet__hswrap(MlirOperationState *state, const char *name_data, size_t name_length, const void *location);
extern const void *mlirTypeParseGet__hswrap(void *ctx, const char *type_data, size_t type_length);

extern const void *mlirAttributeParseGet__hswrap(void *ctx, const char *attr_data, size_t attr_length);
extern const void *mlirIdentifierGet__hswrap(void *ctx, const char *str_data, size_t str_length);

extern const void *mlirOpaqueTypeGet__hswrap(void *ctx, const char *namespace_data, size_t namespace_length, const char *type_data, size_t type_length);
extern void mlirOpaqueTypeGetData__hswrap(void *type, MlirStringRef *output);

extern void mlirDictionaryAttrGetElement__hswrap(const void *attr, intptr_t pos, MlirNamedAttribute *output);
extern const void *mlirDictionaryAttrGetElementByName__hswrap(const void *attr, const char *name_data, size_t name_length);

extern MlirAttribute mlirOpaqueAttrGet__hswrap(MlirContext ctx, const char *namespace_data, size_t namespace_length, intptr_t dataLength, const char *data, MlirType type); 
extern void mlirOpaqueAttrGetDialectNamespace__hswrap(MlirAttribute attr, MlirStringRef *output);
extern void mlirOpaqueAttrGetData__hswrap(MlirAttribute attr, MlirStringRef *output);

extern MlirAttribute mlirStringAttrGet__hswrap(MlirContext context, const char *str_data, size_t str_length);
extern MlirAttribute mlirStringAttrTypedGet__hswrap(MlirType type, const char *str_data, size_t str_length);
extern void mlirStringAttrGetValue__hswrap(MlirAttribute attr, MlirStringRef *output);

extern void mlirIdentifierStr__hswrap(MlirIdentifier ident, MlirStringRef *output);
