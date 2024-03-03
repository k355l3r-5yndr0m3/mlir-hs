#include "wrapper.h"

#include <mlir-c/Support.h>
#include <mlir-c/IR.h>
#include <mlir-c/BuiltinAttributes.h>
#include <mlir-c/BuiltinTypes.h>
#include <stddef.h>

void *mlirContextGetOrLoadDialect__hswrap(void *ctx, const char *name_data, size_t name_length) { return mlirContextGetOrLoadDialect((MlirContext){ctx}, (MlirStringRef){name_data, name_length}).ptr; }
bool mlirContextIsRegisteredOperation__hswrap(void *cxt, const char *name_data, size_t name_length) { return mlirContextIsRegisteredOperation((MlirContext){cxt}, (MlirStringRef){name_data, name_length}); }

void mlirDialectGetNamespace__hswrap(void *dialect, MlirStringRef *output) { *output = mlirDialectGetNamespace((MlirDialect){dialect}); }
void mlirDialectHandleGetNamespace__hswrap(void *dialectHandle, MlirStringRef *output) { *output = mlirDialectHandleGetNamespace((MlirDialectHandle){dialectHandle}); }

const void *mlirModuleCreateParse__hswrap(void *ctx, const char *module_data, size_t module_length) { return mlirModuleCreateParse((MlirContext){ctx}, (MlirStringRef){(const char *)module_data, module_length}).ptr; }

void mlirOperationStateGet__hswrap(MlirOperationState *state, const char *name_data, size_t name_length, const void *location) { *state = mlirOperationStateGet((MlirStringRef){name_data, name_length}, (MlirLocation){location}); }
const void *mlirTypeParseGet__hswrap(void *ctx, const char *type_data, size_t type_length) { return mlirTypeParseGet((MlirContext){ctx}, (MlirStringRef){type_data, type_length}).ptr; }

const void *mlirAttributeParseGet__hswrap(void *ctx, const char *attr_data, size_t attr_length) { return mlirAttributeParseGet((MlirContext){ctx}, (MlirStringRef){attr_data, attr_length}).ptr; }
const void *mlirIdentifierGet__hswrap(void *ctx, const char *str_data, size_t str_length) { return mlirIdentifierGet((MlirContext){ctx}, (MlirStringRef){str_data, str_length}).ptr; }

const void *mlirOpaqueTypeGet__hswrap(void *ctx, const char *namespace_data, size_t namespace_length, const char *type_data, size_t type_length) { return mlirOpaqueTypeGet((MlirContext){ctx}, (MlirStringRef){namespace_data, namespace_length}, (MlirStringRef){type_data, type_length}).ptr; }
void mlirOpaqueTypeGetData__hswrap(void *type, MlirStringRef *output) { *output = mlirOpaqueTypeGetData((MlirType){type}); }
void mlirDictionaryAttrGetElement__hswrap(const void *attr, intptr_t pos, MlirNamedAttribute *output) { *output = mlirDictionaryAttrGetElement((MlirAttribute){attr}, pos); }
const void *mlirDictionaryAttrGetElementByName__hswrap(const void *attr, const char *name_data, size_t name_length) { return mlirDictionaryAttrGetElementByName((MlirAttribute){attr}, (MlirStringRef){name_data, name_length}).ptr; }

MlirAttribute mlirOpaqueAttrGet__hswrap(MlirContext ctx, const char *namespace_data, size_t namespace_length, intptr_t dataLength, const char *data, MlirType type) { return mlirOpaqueAttrGet(ctx, (MlirStringRef){namespace_data, namespace_length}, dataLength, data, type); }
void mlirOpaqueAttrGetDialectNamespace__hswrap(MlirAttribute attr, MlirStringRef *output) { *output = mlirOpaqueAttrGetDialectNamespace(attr); }
void mlirOpaqueAttrGetData__hswrap(MlirAttribute attr, MlirStringRef *output) { *output = mlirOpaqueAttrGetData(attr); }
MlirAttribute mlirStringAttrGet__hswrap(MlirContext context, const char *str_data, size_t str_length) { return mlirStringAttrGet(context, (MlirStringRef){str_data, str_length}); }
MlirAttribute mlirStringAttrTypedGet__hswrap(MlirType type, const char *str_data, size_t str_length) { return mlirStringAttrTypedGet(type, (MlirStringRef){str_data, str_length}); }
void mlirStringAttrGetValue__hswrap(MlirAttribute attr, MlirStringRef *output) { *output = mlirStringAttrGetValue(attr); }

void mlirIdentifierStr__hswrap(MlirIdentifier ident, MlirStringRef *output) { *output = mlirIdentifierStr(ident); }
