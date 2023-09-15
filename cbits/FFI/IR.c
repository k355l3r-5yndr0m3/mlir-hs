#include <mlir-c/Support.h>
#include <mlir-c/IR.h>
#include <mlir-c/BuiltinAttributes.h>

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

size_t sizeof__MlirNamedAttribute() { return sizeof(MlirNamedAttribute); }
size_t alignof__MlirNamedAttribute() { return _Alignof(MlirNamedAttribute); }

void poke__MlirNamedAttribute(MlirIdentifier name, MlirAttribute attr, MlirNamedAttribute *namedAttribute) { *namedAttribute = mlirNamedAttributeGet(name, attr); }
MlirAttribute peek__MlirNamedAttribute__attribute(MlirNamedAttribute *namedAttr) { return namedAttr->attribute; }
MlirIdentifier peek__MlirNamedAttribute__name(MlirNamedAttribute *namedAttr) { return namedAttr->name; }

MlirOperation hs__mlirOperationCreate(const char *name, size_t name_length
                                    , MlirLocation loc
                                    , intptr_t n_results, MlirType const *results
                                    , intptr_t n_operands, MlirValue const *operands
                                    , intptr_t n_regions, MlirRegion const *regions
                                    , intptr_t n_successors, MlirBlock const *successors
                                    , intptr_t n_attributes, MlirNamedAttribute const *attributes
                                    , bool enable_result_type_inference) {
    MlirStringRef _name = { .data = name, .length = name_length };
    MlirOperationState state = mlirOperationStateGet(_name, loc);
    
    mlirOperationStateAddResults(&state, n_results, results);
    mlirOperationStateAddOperands(&state, n_operands, operands);
    mlirOperationStateAddOwnedRegions(&state, n_regions, regions);
    mlirOperationStateAddSuccessors(&state, n_successors, successors);
    mlirOperationStateAddAttributes(&state, n_attributes, attributes);

    return mlirOperationCreate(&state);
}

MlirIdentifier hs__mlirIdentifierGet(MlirContext context, const char *data, size_t length) {
    return mlirIdentifierGet(context, (MlirStringRef){.data = data, .length = length});
}

MlirAttribute hs__mlirAttributeParseGet(MlirContext context, const char *data, size_t length) {
    return mlirAttributeParseGet(context, (MlirStringRef){.data = data, .length = length});
}

// typedef struct {
//     char *buffer;
//     size_t byte_alloc;
//     size_t byte_wrote;
// } hs_bytecode_callback_userdata;
// 
// static void hs__mlirOperationWriteBytecode__callback(MlirStringRef ref, void *userdata) {
//     hs_bytecode_callback_userdata *buffer = userdata;
//     size_t byte_needed = buffer->byte_wrote + ref.length;
//     if (byte_needed > buffer->byte_alloc) {
//         size_t to_alloc = (byte_needed / 64 + 1) * 64;
//         void *new_buffer = realloc(buffer->buffer, to_alloc);
//         if (new_buffer == NULL) {
//             fprintf(stderr, "Cannot allocate buffer for bytecode.\n");
//             return;
//         }
//         buffer->buffer = new_buffer;
//         buffer->byte_alloc = to_alloc;
//     }
//     
//     memcpy(buffer->buffer + buffer->byte_wrote, ref.data, ref.length);
//     buffer->byte_wrote = byte_needed;
// }
// 
// void *hs__mlirOperationWriteBytecode(MlirOperation op, size_t *length) {
//     hs_bytecode_callback_userdata buffer = {
//         .buffer = malloc(64),
//         .byte_alloc = 64,
//         .byte_wrote = 0,
//     };
//     mlirOperationWriteBytecode(op, hs__mlirOperationWriteBytecode__callback, &buffer);
//     *length = buffer.byte_wrote;
//     return realloc(buffer.buffer, buffer.byte_wrote);
// }

typedef struct {
    char *buffer;
    size_t byte_available;
    size_t byte_required;
} hs_bytecode_callback_userdata;
static void hs__mlirOperationWriteBytecode__callback(MlirStringRef ref, void *userdata) {
    hs_bytecode_callback_userdata *data = userdata;
    data->byte_required += ref.length;
    if (data->byte_available < ref.length) 
        ref.length = data->byte_available;
    if (data->byte_available == 0)
        return;
    memcpy(data->buffer, ref.data, ref.length);
    data->buffer += ref.length;
    data->byte_available -= ref.length;
}

size_t hs__mlirOperationWriteBytecode(MlirOperation op, size_t buffersize, char *buffer) {
    hs_bytecode_callback_userdata userdata = {
        .buffer = buffer,
        .byte_available = buffersize,
        .byte_required = 0,
    };
    mlirOperationWriteBytecode(op, hs__mlirOperationWriteBytecode__callback, &userdata);
    return userdata.byte_required;
}


// typedef struct {
//     char *buffer;
//     size_t olddatalen;
//     size_t buffersize;
//     size_t requiredsize;
// } buffer_writer_data;
// static void buffer_writer_callback(MlirStringRef ref, void *userdata) {
//     buffer_writer_data *args = userdata;
//     args->requiredsize += ref.length;
//     
//     // if segment is already writen last time, then skip
//     if (args->olddatalen) {
//         size_t skip = ref.length < args->olddatalen ? ref.length : args->olddatalen;
//         ref.length -= skip;
//         ref.data += skip;
// 
//         skip = skip < args->buffersize ? skip : args->buffersize;
//         args->buffersize -= skip;
//         args->buffer += skip;
//     }
// 
//     // buffer should ptr to undefined memory (i.e no writen data)
//     if (args->buffersize == 0 || ref.length == 0) 
//         return;
//     
//     // writing
//     if (args->buffersize < ref.length) {
//         memcpy(args->buffer, ref.data, args->buffersize);
//         args->buffersize = 0;
//     } else {
//         memcpy(args->buffer, ref.data, ref.length);
//         args->buffersize -= ref.length;
//         args->buffer += ref.length;
//     }
// }
// 
// // NOTE: The cost of moving buffer might be bigger than the cost of invoking this function twice
// // TODO: Profile
// size_t write_bytecode(MlirOperation op, size_t olddatalen, size_t buffersize, void *buffer) {
//     buffer_writer_data data = {
//         .buffer = buffer, .olddatalen = olddatalen, .buffersize = buffersize, .requiredsize = 0
//     };
//     mlirOperationWriteBytecode(op, buffer_writer_callback, &data);
//     return data.requiredsize;
// }





