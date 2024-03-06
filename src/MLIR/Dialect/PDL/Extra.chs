module MLIR.Dialect.PDL.Extra
( mlirTypeIsAPDLType             
, mlirTypeIsAPDLAttributeType    
, mlirPDLAttributeTypeGet        
, mlirTypeIsAPDLOperationType    
, mlirPDLOperationTypeGet        
, mlirTypeIsAPDLRangeType        
, mlirPDLRangeTypeGet            
, mlirPDLRangeTypeGetElementType 
, mlirTypeIsAPDLTypeType         
, mlirPDLTypeTypeGet             
, mlirTypeIsAPDLValueType        
, mlirPDLValueTypeGet            
) where
{#import MLIR.FFI.IR #}
import MLIR.FFI.Marshal
import Data.Coerce 

#include <mlir-c/Dialect/PDL.h>

{#fun pure unsafe mlirTypeIsAPDLType              { coerce `MlirType' } -> `Bool' #}
                                                  
{#fun pure unsafe mlirTypeIsAPDLAttributeType     { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirPDLAttributeTypeGet         { coerce `MlirContext' } -> `MlirType' coerce #}
                                                  
{#fun pure unsafe mlirTypeIsAPDLOperationType     { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirPDLOperationTypeGet         { coerce `MlirContext' } -> `MlirType' coerce #}
                                                  
{#fun pure unsafe mlirTypeIsAPDLRangeType         { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirPDLRangeTypeGet             { coerce `MlirType' } -> `MlirType' coerce #}
{#fun pure unsafe mlirPDLRangeTypeGetElementType  { coerce `MlirType' } -> `MlirType' coerce #}

{#fun pure unsafe mlirTypeIsAPDLTypeType          { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirPDLTypeTypeGet              { coerce `MlirContext' } -> `MlirType' coerce #}

{#fun pure unsafe mlirTypeIsAPDLValueType         { coerce `MlirType' } -> `Bool' #}
{#fun pure unsafe mlirPDLValueTypeGet             { coerce `MlirContext' } -> `MlirType' coerce #}

