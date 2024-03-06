module MLIR.FFI.IntegerSet
( mlirIntegerSetGetContext
, mlirIntegerSetIsNull
)where

{#import MLIR.FFI.IR #}

import Data.Void
import Foreign

newtype MlirIntegerSet = MlirIntegerSet (Ptr Void)

{#fun pure unsafe mlirIntegerSetGetContext { coerce `MlirIntegerSet' } -> `MlirContext' coerce #}

mlirIntegerSetIsNull :: MlirIntegerSet -> Bool
mlirIntegerSetIsNull (MlirIntegerSet ptr) = ptr == nullPtr
