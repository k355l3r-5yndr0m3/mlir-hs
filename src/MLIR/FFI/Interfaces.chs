module MLIR.FFI.Interfaces
( mlirOperationImplementsInterface
, mlirInferTypeOpInterfaceTypeID
) where

{#import MLIR.FFI.Support #}
{#import MLIR.FFI.IR #}

import Data.Coerce

#include <mlir-c/Interfaces.h>

{#fun pure unsafe mlirOperationImplementsInterface { coerce `MlirOperation', coerce `MlirTypeID' } -> `Bool' #}
-- TODO: Implement mlirOperationImplementsInterfaceStatic

{#fun pure unsafe mlirInferTypeOpInterfaceTypeID { } -> `MlirTypeID' coerce #}
