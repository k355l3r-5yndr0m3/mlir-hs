module MLIR.FFI.AffineExpr where
import MLIR.FFI.IR

import Data.Int
import Data.Coerce
import Data.Void
import Foreign

#include <mlir-c/AffineExpr.h>

newtype MlirAffineExpr = MlirAffineExpr (Ptr Void)
newtype MlirAffineMap = MlirAffineMap (Ptr Void)

{#fun pure unsafe mlirAffineExprGetContext { coerce `MlirAffineExpr' } -> `MlirContext' coerce #}
{#fun pure unsafe mlirAffineExprEqual { coerce `MlirAffineExpr', coerce `MlirAffineExpr' } -> `Bool' #} 

mlirAffineExprIsNull :: MlirAffineExpr -> Bool
mlirAffineExprIsNull (MlirAffineExpr ptr) = ptr == nullPtr

{#fun unsafe mlirAffineExprDump { coerce `MlirAffineExpr' } -> `()' #}
{#fun pure unsafe mlirAffineExprIsSymbolicOrConstant { coerce `MlirAffineExpr' } -> `Bool' #}
{#fun pure unsafe mlirAffineExprIsPureAffine { coerce `MlirAffineExpr' } -> `Bool' #}

{#fun pure unsafe mlirAffineExprGetLargestKnownDivisor { coerce `MlirAffineExpr' } -> `Int64' #}
{#fun pure unsafe mlirAffineExprIsMultipleOf { coerce `MlirAffineExpr', `Int64' } -> `Bool' #}

{#fun pure unsafe mlirAffineExprIsFunctionOfDim { coerce `MlirAffineExpr', `Int64' } -> `Bool' #}
{#fun pure unsafe mlirAffineExprCompose { coerce `MlirAffineExpr', coerce `MlirAffineMap' } -> `MlirAffineExpr' coerce #}

{#fun pure unsafe mlirAffineExprIsADim { coerce `MlirAffineExpr' } -> `Bool' #}
{#fun pure unsafe mlirAffineDimExprGet { coerce `MlirContext', `Int' } -> `MlirAffineExpr' coerce #}
{#fun pure unsafe mlirAffineDimExprGetPosition { coerce `MlirAffineExpr' } -> `Int' #}

{#fun pure unsafe mlirAffineExprIsASymbol { coerce `MlirAffineExpr' } -> `Bool' #}
{#fun pure unsafe mlirAffineSymbolExprGet { coerce `MlirContext', `Int' } -> `MlirAffineExpr' coerce #}
{#fun pure unsafe mlirAffineSymbolExprGetPosition { coerce `MlirAffineExpr' } -> `Int' #}

{#fun pure unsafe mlirAffineExprIsAConstant { coerce `MlirAffineExpr' } -> `Bool' #}
{#fun pure unsafe mlirAffineConstantExprGet { coerce `MlirContext', coerce `Int64' } -> `MlirAffineExpr' coerce #}
{#fun pure unsafe mlirAffineConstantExprGetValue { coerce `MlirAffineExpr' } -> `Int64' coerce #}

{#fun pure unsafe mlirAffineExprIsAAdd { coerce `MlirAffineExpr' } -> `Bool' #}
{#fun pure unsafe mlirAffineAddExprGet { coerce `MlirAffineExpr', coerce `MlirAffineExpr' } -> `MlirAffineExpr' coerce #}

{#fun pure unsafe mlirAffineExprIsAMul { coerce `MlirAffineExpr' } -> `Bool' #}
{#fun pure unsafe mlirAffineMulExprGet { coerce `MlirAffineExpr', coerce `MlirAffineExpr' } -> `MlirAffineExpr' coerce #}

{#fun pure unsafe mlirAffineExprIsAMod { coerce `MlirAffineExpr' } -> `Bool' #}
{#fun pure unsafe mlirAffineModExprGet { coerce `MlirAffineExpr', coerce `MlirAffineExpr' } -> `MlirAffineExpr' coerce #}

{#fun pure unsafe mlirAffineExprIsAFloorDiv { coerce `MlirAffineExpr' } -> `Bool' #}
{#fun pure unsafe mlirAffineFloorDivExprGet { coerce `MlirAffineExpr', coerce `MlirAffineExpr' } -> `MlirAffineExpr' coerce #}

{#fun pure unsafe mlirAffineExprIsACeilDiv { coerce `MlirAffineExpr' } -> `Bool' #}
{#fun pure unsafe mlirAffineCeilDivExprGet { coerce `MlirAffineExpr', coerce `MlirAffineExpr' } -> `MlirAffineExpr' coerce #}

{#fun pure unsafe mlirAffineExprIsABinary { coerce `MlirAffineExpr' } -> `Bool' #}
{#fun pure unsafe mlirAffineBinaryOpExprGetLHS { coerce `MlirAffineExpr' } -> `MlirAffineExpr' coerce #}
{#fun pure unsafe mlirAffineBinaryOpExprGetRHS { coerce `MlirAffineExpr' } -> `MlirAffineExpr' coerce #}
