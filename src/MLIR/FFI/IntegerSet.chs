module MLIR.FFI.IntegerSet
( mlirIntegerSetGetContext
, mlirIntegerSetIsNull
)where

{#import MLIR.FFI.IR #}

import Data.Void
import Data.Coerce
import Foreign

#include <mlir-c/IntegerSet.h>

newtype MlirIntegerSet = MlirIntegerSet (Ptr Void)

{#fun pure unsafe mlirIntegerSetGetContext { coerce `MlirIntegerSet' } -> `MlirContext' coerce #}

mlirIntegerSetIsNull :: MlirIntegerSet -> Bool
mlirIntegerSetIsNull (MlirIntegerSet ptr) = ptr == nullPtr
