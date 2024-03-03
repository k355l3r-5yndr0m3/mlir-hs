{-# OPTIONS_GHC -W #-}
module MLIR.FFI.IR 
( MlirContext (..)
, MlirDialect (..)
, MlirDialectRegistry (..)
, MlirDialectHandle (..)
, MlirLocation (..)
, MlirAttribute (..)
, MlirModule (..)
, MlirBlock (..)
, MlirOperation (..)
, MlirIdentifier (..)
, MlirType (..)
, MlirValue (..)
, MlirRegion (..)
, MlirOpOperand (..)
, MlirNamedAttribute (..)
, MlirTypePtr
, MlirAttributePtr
, MlirNamedAttributePtr 

, mlirContextCreate
, mlirContextCreateWithThreading
, mlirContextCreateWithRegistry
, mlirContextEqual
, mlirContextSetAllowUnregisteredDialects
, mlirContextGetAllowUnregisteredDialects
, mlirContextIsNull
, mlirContextGetNumRegisteredDialects
, mlirContextAppendDialectRegistry
, mlirContextDestroy
, mlirContextGetNumLoadedDialects
, mlirContextGetOrLoadDialect
, mlirContextEnableMultithreading
, mlirContextLoadAllAvailableDialects
, mlirContextIsRegisteredOperation

, mlirDialectGetContext
, mlirDialectGetNamespace
, mlirDialectHandleGetNamespace
, mlirDialectIsNull
, mlirDialectHandleInsertDialect
, mlirDialectHandleRegisterDialect
, mlirDialectHandleLoadDialect
, mlirDialectEqual
, mlirDialectRegistryCreate
, mlirDialectRegistryIsNull
, mlirDialectRegistryDestroy

, mlirLocationGetAttribute
, mlirLocationFromAttribute
, mlirLocationCallSiteGet
, mlirLocationIsNull
, mlirLocationUnknownGet
, mlirLocationGetContext
, mlirLocationEqual

, mlirModuleCreateEmpty
, mlirModuleCreateParse
, mlirModuleDestroy
, mlirModuleGetContext
, mlirModuleGetBody
, mlirModuleFromOperation
, mlirModuleIsNull
, mlirModuleGetOperation

, mlirOperationCreate
, mlirOperationDestroy
, mlirOperationClone
, mlirOperationRemoveFromParent
, mlirOperationGetContext
, mlirOperationIsNull
, mlirOperationGetLocation
, mlirOperationDump
, mlirOperationEqual
, mlirOperationVerify
, mlirOperationGetFirstRegion
-- Some are missing
, mlirOperationGetNextInBlock
, mlirOperationGetNumResults
, mlirOperationGetResult
, mlirOperationGetNumOperands
, mlirOperationGetOperand
, mlirOperationGetNumRegions
, mlirOperationGetRegion
, mlirOperationSetAttributeByName
, mlirOperationRemoveAttributeByName
, mlirOperationGetAttributeByName
, mlirOperationGetAttribute
, mlirOperationGetNumAttributes
, mlirOperationGetParentOperation
, mlirOperationGetSuccessor
, mlirOperationGetNumSuccessors
, mlirOperationSetOperand
, mlirOperationPrint
, mlirOperationWriteBytecode
, mlirOperationMoveAfter
, mlirOperationMoveBefore

, mlirRegionDestroy
, mlirRegionIsNull
, mlirRegionEqual
, mlirRegionCreate
, mlirRegionGetFirstBlock
, mlirRegionAppendOwnedBlock
, mlirRegionInsertOwnedBlock
, mlirRegionInsertOwnedBlockAfter
, mlirRegionInsertOwnedBlockBefore
, mlirRegionTakeBody
, mlirRegionGetNextInOperation

, mlirBlockCreate
, mlirBlockIsNull
, mlirBlockDestroy
, mlirBlockEqual
, mlirBlockDetach
, mlirBlockGetParentOperation
, mlirBlockGetFirstOperation
, mlirBlockAppendOwnedOperation
, mlirBlockInsertOwnedOperation
, mlirBlockInsertOwnedOperationAfter
, mlirBlockGetParentRegion
, mlirBlockInsertOwnedOperationBefore
, mlirBlockGetNumArguments
, mlirBlockAddArgument
, mlirBlockInsertArgument
, mlirBlockGetArgument
, mlirBlockGetNextInRegion

, mlirValueIsNull
, mlirValueEqual
, mlirValueGetType
, mlirValueDump

, mlirValueIsABlockArgument
, mlirBlockArgumentGetOwner
, mlirBlockArgumentGetArgNumber
, mlirBlockArgumentSetType

, mlirValueIsAOpResult
, mlirOpResultGetOwner
, mlirOpResultGetResultNumber

, mlirOpOperandIsNull
, mlirOpOperandGetOwner
, mlirOpOperandGetNextUse
, mlirOpOperandGetOperandNumber

, mlirTypeParseGet
, mlirTypeGetTypeID
, mlirTypeGetDialect
, mlirTypeIsNull
, mlirTypeEqual
, mlirTypeGetContext
, mlirTypeDump

, mlirAttributeParseGet
, mlirAttributeGetContext
, mlirAttributeGetType
, mlirAttributeGetTypeID
, mlirAttributeGetDialect
, mlirAttributeIsNull
, mlirAttributeEqual
, mlirAttributeDump
, mlirNamedAttributeGet

, mlirIdentifierStr
, mlirIdentifierGetContext
, mlirIdentifierGet
, mlirIdentifierEqual
) where
#include <mlir-c/IR.h>
#include <wrapper.h>
{#import MLIR.FFI.Support #}
import MLIR.FFI.Marshal
import Control.Monad (when)

import Data.Void
import Data.Coerce

import Foreign.C
import Foreign
-- This module serve as a thin binding of the mlir c api

-- This assume that a C struct containing a void pointer is identical to a void pointer in the ABI
newtype MlirContext = MlirContext (Ptr Void)
newtype MlirDialect = MlirDialect (Ptr Void)
newtype MlirDialectRegistry = MlirDialectRegistry (Ptr Void)
newtype MlirDialectHandle = MlirDialectHandle (Ptr Void)
newtype MlirLocation = MlirLocation (Ptr Void) deriving Storable
newtype MlirAttribute = MlirAttribute (Ptr Void) deriving Storable
newtype MlirModule = MlirModule (Ptr Void)
newtype MlirBlock = MlirBlock (Ptr Void) deriving Storable
newtype MlirOperation = MlirOperation (Ptr Void)
newtype MlirIdentifier = MlirIdentifier (Ptr Void)
newtype MlirType = MlirType (Ptr Void) deriving Storable
newtype MlirValue = MlirValue (Ptr Void) deriving Storable
newtype MlirRegion = MlirRegion (Ptr Void) deriving Storable
newtype MlirOpOperand = MlirOpOperand (Ptr Void)

type MlirTypePtr = Ptr MlirType
{#pointer *MlirType as MlirTypePtr nocode #}
type MlirAttributePtr = Ptr MlirAttribute
{#pointer *MlirAttribute as MlirAttributePtr nocode #}
type MlirNamedAttributePtr = Ptr MlirNamedAttribute
{#pointer *MlirNamedAttribute as MlirNamedAttributePtr nocode #}

data MlirNamedAttribute = MlirNamedAttribute
  { mlirNamedAttributeName      :: !MlirIdentifier
  , mlirNamedAttributeAttribute :: !MlirAttribute
  }
instance Storable MlirNamedAttribute where
  sizeOf _ = {#sizeof MlirNamedAttribute#}
  alignment _ = {#alignof MlirNamedAttribute#}

  peek p = MlirNamedAttribute <$> fmap (MlirIdentifier . castPtr) ({#get MlirNamedAttribute->name#} p)
                              <*> fmap (MlirAttribute . castPtr) ({#get MlirNamedAttribute->attribute#} p)
  poke p (MlirNamedAttribute { mlirNamedAttributeName      = MlirIdentifier name
                               , mlirNamedAttributeAttribute = MlirAttribute  attr
                               }) = {#set MlirNamedAttribute->name#} p (castPtr name)
                                 >> {#set MlirNamedAttribute->attribute#} p (castPtr attr)

{#fun unsafe mlirContextCreate { } -> `MlirContext' coerce #}
{#fun unsafe mlirContextCreateWithThreading { `Bool' } -> `MlirContext' coerce #}
{#fun unsafe mlirContextCreateWithRegistry { coerce `MlirDialectRegistry', `Bool' } -> `MlirContext' coerce #}
{#fun unsafe mlirContextEqual { coerce `MlirContext', coerce `MlirContext' } -> `Bool' #} -- This might be pure

mlirContextIsNull :: MlirContext -> Bool
mlirContextIsNull (MlirContext ctx) = nullPtr == ctx

{#fun unsafe mlirContextDestroy { coerce `MlirContext' } -> `()' #}

{#fun unsafe mlirContextSetAllowUnregisteredDialects { coerce `MlirContext', `Bool' } -> `()' #}
{#fun unsafe mlirContextGetAllowUnregisteredDialects { coerce `MlirContext' } -> `Bool' #}

{#fun unsafe mlirContextGetNumRegisteredDialects { coerce `MlirContext' } -> `Int' #}
{#fun unsafe mlirContextAppendDialectRegistry { coerce `MlirContext', coerce `MlirDialectRegistry' } -> `()' #}
{#fun unsafe mlirContextGetNumLoadedDialects { coerce `MlirContext' } -> `Int' #}

{#fun unsafe mlirContextGetOrLoadDialect__hswrap as mlirContextGetOrLoadDialect { coerce `MlirContext', `String'& } -> `MlirDialect' coerce #}
{#fun unsafe mlirContextEnableMultithreading { coerce `MlirContext', `Bool' } -> `()' #}
{#fun unsafe mlirContextLoadAllAvailableDialects { coerce `MlirContext' } -> `()' #}
{#fun unsafe mlirContextIsRegisteredOperation__hswrap as mlirContextIsRegisteredOperation { coerce `MlirContext', `String'& } -> `Bool' #}
-- TODO: mlirContextSetThreadPool
{#fun pure unsafe mlirDialectGetContext { coerce `MlirDialect' } -> `MlirContext' coerce #} -- not IO because dialect cannot change context

mlirDialectIsNull :: MlirDialect -> Bool
mlirDialectIsNull (MlirDialect dialect) = dialect == nullPtr
-- Probably does not need IO
{#fun pure unsafe mlirDialectEqual { coerce `MlirDialect', coerce `MlirDialect' } -> `Bool' #}
{#fun pure unsafe mlirDialectGetNamespace__hswrap as mlirDialectGetNamespace { coerce `MlirDialect', alloca- `String' peekStringRefPtr* } -> `()' #}
{#fun pure unsafe mlirDialectHandleGetNamespace__hswrap as mlirDialectHandleGetNamespace { coerce `MlirDialectHandle', alloca- `String' peekStringRefPtr* } -> `()' #}

{#fun unsafe mlirDialectHandleInsertDialect { coerce `MlirDialectHandle', coerce `MlirDialectRegistry' } -> `()' #}
{#fun unsafe mlirDialectHandleRegisterDialect { coerce `MlirDialectHandle', coerce `MlirContext' } -> `()' #}
{#fun unsafe mlirDialectHandleLoadDialect { coerce `MlirDialectHandle', coerce `MlirContext' } -> `MlirDialect' coerce #}

{#fun unsafe mlirDialectRegistryCreate {} -> `MlirDialectRegistry' coerce #}
mlirDialectRegistryIsNull :: MlirDialectRegistry -> Bool
mlirDialectRegistryIsNull (MlirDialectRegistry dialectRegistry) = dialectRegistry == nullPtr
{#fun unsafe mlirDialectRegistryDestroy { coerce `MlirDialectRegistry' } -> `()' #}

{- MLIR Location API
 - -}
{#fun pure unsafe mlirLocationGetAttribute { coerce `MlirLocation' } -> `MlirAttribute' coerce #}
{#fun unsafe mlirLocationFromAttribute { coerce `MlirAttribute' } -> `MlirLocation' coerce #}
{#fun unsafe mlirLocationCallSiteGet { coerce `MlirLocation', coerce `MlirLocation' } -> `MlirLocation' coerce #}

mlirLocationIsNull :: MlirLocation -> Bool
mlirLocationIsNull (MlirLocation location) = location == nullPtr

{#fun pure unsafe mlirLocationUnknownGet { coerce `MlirContext' } -> `MlirLocation' coerce #}
{#fun pure unsafe mlirLocationGetContext { coerce `MlirLocation' } -> `MlirContext' coerce #}
{#fun pure unsafe mlirLocationEqual { coerce `MlirLocation', coerce `MlirLocation' } -> `Bool' #}
{- MLIR Module API
 - -}
{#fun unsafe mlirModuleCreateEmpty { coerce `MlirLocation' } -> `MlirModule' coerce #}
{#fun unsafe mlirModuleCreateParse__hswrap as mlirModuleCreateParse { coerce `MlirContext', marshalByteArraySwap* `ByteArray'& } -> `MlirModule' coerce #}
{#fun unsafe mlirModuleDestroy { coerce `MlirModule' } -> `()' #}
{#fun pure unsafe mlirModuleGetContext { coerce `MlirModule' } -> `MlirContext' coerce #}
{#fun pure unsafe mlirModuleGetBody { coerce `MlirModule' } -> `MlirBlock' coerce #} -- Should be fine since operation are mostly inmutable

mlirModuleIsNull :: MlirModule -> Bool
mlirModuleIsNull (MlirModule m) = m == nullPtr
-- These two function should be pure
{#fun pure unsafe mlirModuleGetOperation { coerce `MlirModule' } -> `MlirOperation' coerce #}
{#fun pure unsafe mlirModuleFromOperation { coerce `MlirOperation' } -> `MlirModule' coerce #}

{- Operation state to create operation
 - -}
type MlirOperationStatePtr = Ptr MlirOperationState
data MlirOperationState
{#pointer *MlirOperationState as MlirOperationStatePtr nocode #}
instance Storable MlirOperationState where
  sizeOf _ = {#sizeof MlirOperationState#}
  alignment _ = {#alignof MlirOperationState#}

  peek _ = undefined
  poke _ _ = return ()

{#fun unsafe mlirOperationStateGet__hswrap as mlirOperationStateInit { `MlirOperationStatePtr', `String'&, coerce `MlirLocation' } -> `()' #}
{#fun unsafe mlirOperationStateAddResults { `MlirOperationStatePtr', marshalStorableArrayLenCast* `[MlirType]'& } -> `()' #}
{#fun unsafe mlirOperationStateAddOperands { `MlirOperationStatePtr', marshalStorableArrayLenCast* `[MlirValue]'& } -> `()' #}
{#fun unsafe mlirOperationStateAddOwnedRegions { `MlirOperationStatePtr', marshalStorableArrayLenCast* `[MlirRegion]'& } -> `()' #}
{#fun unsafe mlirOperationStateAddSuccessors { `MlirOperationStatePtr', marshalStorableArrayLenCast* `[MlirBlock]'& } -> `()' #}
{#fun unsafe mlirOperationStateAddAttributes { `MlirOperationStatePtr', marshalStorableArrayLenCast* `[MlirNamedAttribute]'& } -> `()' #}
{#fun unsafe mlirOperationStateEnableResultTypeInference { `MlirOperationStatePtr' } -> `()' #}

{- Operation API
 - -}
foreign import ccall unsafe "mlirOperationCreate"
  mlirOperationCreate_ :: Ptr MlirOperationState -> IO MlirOperation
mlirOperationCreate :: String -> MlirLocation -> [MlirType] -> [MlirValue] -> [MlirRegion] -> [MlirBlock] -> [MlirNamedAttribute] -> Bool -> IO MlirOperation
mlirOperationCreate opname location results operands regions successors attributes enableResultTypeInference = alloca $ \ state -> do
  mlirOperationStateInit state opname location
  mlirOperationStateAddResults state results
  mlirOperationStateAddOperands state operands 
  mlirOperationStateAddOwnedRegions state regions
  mlirOperationStateAddSuccessors state successors
  mlirOperationStateAddAttributes state attributes
  when enableResultTypeInference $ mlirOperationStateEnableResultTypeInference state
  mlirOperationCreate_ state

{#fun unsafe mlirOperationDestroy { coerce `MlirOperation' } -> `()' #}
{#fun unsafe mlirOperationClone { coerce `MlirOperation' } -> `MlirOperation' coerce #}
{#fun unsafe mlirOperationRemoveFromParent { coerce `MlirOperation' } -> `()' #} -- MUTATE A BLOCK BUT DOES NOT REPLACE IT

mlirOperationIsNull :: MlirOperation -> Bool
mlirOperationIsNull (MlirOperation operation) = operation == nullPtr

{#fun pure unsafe mlirOperationEqual { coerce `MlirOperation', coerce `MlirOperation' } -> `Bool' #}
{#fun pure unsafe mlirOperationGetContext { coerce `MlirOperation' } -> `MlirContext' coerce #}
{#fun pure unsafe mlirOperationGetLocation { coerce `MlirOperation' } -> `MlirLocation' coerce #}

{#fun unsafe mlirOperationDump { coerce `MlirOperation' } -> `()' #}
{#fun unsafe mlirOperationVerify { coerce `MlirOperation' } -> `Bool' #} -- Depend on how mutable an operation is, this could be pure

-- These two are probably pure (it is not documented as such though)
{#fun pure unsafe mlirOperationGetNumResults { coerce `MlirOperation' } -> `Int' #}
{#fun pure unsafe mlirOperationGetResult { coerce `MlirOperation', `Int' } -> `MlirValue' coerce #}

-- These are not pure since they can change
-- But their types and number is fixed (again not documented, dug through the code)
{#fun pure unsafe mlirOperationGetNumOperands { coerce `MlirOperation' } -> `Int' #}
{#fun unsafe mlirOperationGetOperand { coerce `MlirOperation', `Int' } -> `MlirValue' coerce #}
{#fun unsafe mlirOperationSetOperand { coerce `MlirOperation', `Int', coerce `MlirValue' } -> `()' #}
-- There is a mlirOperationSetOperands function which allow setting multiple operands
-- But this is quite poorly documented. The number of operands is constant after creation (not doc, dug through code)
-- So probably this only set the lower indexed operands (which is very odd)

-- There does not exist a way to set an operation's region after creation
-- It is possible to mutate a region but not change that region with another region in an operation
{#fun pure unsafe mlirOperationGetNumRegions { coerce `MlirOperation' } -> `Int' #}
{#fun pure unsafe mlirOperationGetRegion { coerce `MlirOperation', `Int' } -> `MlirRegion' coerce #}

-- This version of mlir does not allow modification of successors
-- Although newer versions allow it
{#fun pure unsafe mlirOperationGetNumSuccessors { coerce `MlirOperation' } -> `Int' #}
{#fun pure unsafe mlirOperationGetSuccessor { coerce `MlirOperation', `Int' } -> `MlirBlock' coerce #}

{#fun unsafe mlirOperationGetNextInBlock { coerce `MlirOperation' } -> `MlirOperation' coerce #}
{#fun unsafe mlirOperationGetParentOperation { coerce `MlirOperation' } -> `MlirOperation' coerce #}

{#fun unsafe mlirOperationGetNumAttributes { coerce `MlirOperation' } -> `Int' #}
#c
void mlirOperationGetAttribute__hswrap(MlirOperation op, intptr_t pos, MlirNamedAttribute *output) {
    *output = mlirOperationGetAttribute(op, pos);
}
#endc
{#fun unsafe mlirOperationGetAttribute__hswrap as mlirOperationGetAttribute { coerce `MlirOperation', `Int', alloca- `MlirNamedAttribute' peek* } -> `()' #}

#c
MlirAttribute mlirOperationGetAttributeByName__hswrap(MlirOperation op, const char *name_data, size_t name_length) {
    mlirOperationGetAttributeByName(op, (MlirStringRef){name_data, name_length});
}
#endc
{#fun unsafe mlirOperationGetAttributeByName__hswrap as mlirOperationGetAttributeByName { coerce `MlirOperation', `String'& } -> `MlirOperation' coerce #}


#c
bool mlirOperationRemoveAttributeByName__hswrap(MlirOperation op, const char *name_data, size_t name_length) {
    return mlirOperationRemoveAttributeByName(op, (MlirStringRef){name_data, name_length});
}
#endc

{#fun unsafe mlirOperationRemoveAttributeByName__hswrap as mlirOperationRemoveAttributeByName { coerce `MlirOperation', `String'& } -> `Bool' #}

#c
void mlirOperationSetAttributeByName__hswrap(MlirOperation op, const char *name_data, size_t name_length, MlirAttribute attr) {
    mlirOperationSetAttributeByName(op, (MlirStringRef){name_data, name_length}, attr);
}
#endc
{#fun unsafe mlirOperationSetAttributeByName__hswrap as mlirOperationSetAttributeByName { coerce `MlirOperation', `String'&, coerce `MlirAttribute' } -> `()' #}

{#fun mlirOperationPrint { coerce `MlirOperation', withMlirStringCallback* `MlirStringCallback'& } -> `()' #}
{#fun mlirOperationWriteBytecode { coerce `MlirOperation', withMlirStringCallback* `MlirStringCallback'& } -> `()' #}

{#fun unsafe mlirOperationMoveAfter { coerce `MlirOperation', coerce `MlirOperation' } -> `()' #}
{#fun unsafe mlirOperationMoveBefore { coerce `MlirOperation', coerce `MlirOperation' } -> `()' #}

{- Region API
 - -}
{#fun unsafe mlirRegionCreate { } -> `MlirRegion' coerce #}
{#fun unsafe mlirRegionDestroy { coerce `MlirRegion' } -> `()' #}

mlirRegionIsNull :: MlirRegion -> Bool
mlirRegionIsNull (MlirRegion region) = region == nullPtr

{#fun pure unsafe mlirRegionEqual { coerce `MlirRegion', coerce `MlirRegion' } -> `Bool' #}

{#fun unsafe mlirRegionGetFirstBlock { coerce `MlirRegion' } -> `MlirBlock' coerce #}
{#fun unsafe mlirRegionAppendOwnedBlock { coerce `MlirRegion', coerce `MlirBlock' } -> `()' #}
{#fun unsafe mlirRegionInsertOwnedBlock { coerce `MlirRegion', `Int', coerce `MlirBlock' } -> `()' #}
{#fun unsafe mlirRegionInsertOwnedBlockAfter { coerce `MlirRegion', coerce `MlirBlock', coerce `MlirBlock' } -> `()' #}
{#fun unsafe mlirRegionInsertOwnedBlockBefore { coerce `MlirRegion', coerce `MlirBlock', coerce `MlirBlock' } -> `()' #}

{#fun pure unsafe mlirOperationGetFirstRegion { coerce `MlirOperation' } -> `MlirRegion' coerce #} -- Regions in a operation can possible mutate, but is still the same object or so it seems
{#fun pure unsafe mlirRegionGetNextInOperation { coerce `MlirRegion' } -> `MlirRegion' coerce #}
{#fun unsafe mlirRegionTakeBody { coerce `MlirRegion', coerce `MlirRegion' } -> `()' #} -- Mutate the regions but does not replace them or invalidate them

{- Block API
 - -}

foreign import ccall unsafe "mlirBlockCreate"
  mlirBlockCreate_ :: CIntPtr -> Ptr MlirType -> Ptr MlirLocation -> IO MlirBlock
mlirBlockCreate :: [(MlirType, MlirLocation)] -> IO MlirBlock
mlirBlockCreate arglocs =
  withArrayLen args (\ nargs args' -> 
    withArray locs (\ locs' -> 
      mlirBlockCreate_ (fromIntegral nargs) args' locs'
      )
    )
  where (args, locs) = unzip arglocs

foreign import ccall unsafe "mlirBlockDestroy"
  mlirBlockDestroy :: MlirBlock -> IO ()

{#fun unsafe mlirBlockDetach { coerce `MlirBlock' } -> `()' #}

mlirBlockIsNull :: MlirBlock -> Bool
mlirBlockIsNull (MlirBlock block) = block == nullPtr

{#fun unsafe mlirBlockEqual { coerce `MlirBlock', coerce `MlirBlock' } -> `Bool' #}
{#fun unsafe mlirBlockGetParentOperation { coerce `MlirBlock' } -> `MlirOperation' coerce #} -- not pure because it can change
{#fun unsafe mlirBlockGetParentRegion { coerce `MlirBlock' } -> `MlirRegion' coerce #} -- likewise
{#fun unsafe mlirBlockGetNextInRegion { coerce `MlirBlock' } -> `MlirBlock' coerce #}
{#fun unsafe mlirBlockGetFirstOperation { coerce `MlirBlock' } -> `MlirOperation' coerce #}
{#fun unsafe mlirBlockAppendOwnedOperation { coerce `MlirBlock', coerce `MlirOperation' } -> `()' #}
{#fun unsafe mlirBlockInsertOwnedOperation { coerce `MlirBlock', `Int', coerce `MlirOperation' } -> `()' #}
{#fun unsafe mlirBlockInsertOwnedOperationAfter { coerce `MlirBlock', coerce `MlirOperation', coerce `MlirOperation' } -> `()' #}
{#fun unsafe mlirBlockInsertOwnedOperationBefore { coerce `MlirBlock', coerce `MlirOperation', coerce `MlirOperation' } -> `()' #}

-- NOTE: It is impossible to remove an argument
{#fun unsafe mlirBlockGetNumArguments { coerce `MlirBlock' } -> `Int' #}
{#fun unsafe mlirBlockAddArgument { coerce `MlirBlock', coerce `MlirType', coerce `MlirLocation' } -> `MlirValue' coerce #}
{#fun unsafe mlirBlockInsertArgument { coerce `MlirBlock', `Int', coerce `MlirType', coerce `MlirLocation' } -> `MlirValue' coerce #}
{#fun unsafe mlirBlockGetArgument { coerce `MlirBlock', `Int' } -> `MlirValue' coerce #}

{- Value API
 - -}
mlirValueIsNull :: MlirValue -> Bool
mlirValueIsNull (MlirValue value) = value == nullPtr

{#fun pure unsafe mlirValueEqual { coerce `MlirValue', coerce `MlirValue' } -> `Bool' #}
{#fun pure unsafe mlirValueIsABlockArgument { coerce `MlirValue' } -> `Bool' #}
{#fun pure unsafe mlirValueIsAOpResult { coerce `MlirValue' } -> `Bool' #}

{#fun pure unsafe mlirBlockArgumentGetOwner { coerce `MlirValue' } -> `MlirBlock' coerce #}
{#fun unsafe mlirBlockArgumentGetArgNumber { coerce `MlirValue' } -> `Int' #}
{#fun unsafe mlirBlockArgumentSetType { coerce `MlirValue', coerce `MlirType' } -> `()' #}

{#fun pure unsafe mlirOpResultGetOwner { coerce `MlirValue' } -> `MlirOperation' coerce #}
{#fun pure unsafe mlirOpResultGetResultNumber { coerce `MlirValue' } -> `Int' #} -- Should to pure

{#fun unsafe mlirValueGetType { coerce `MlirValue' } -> `MlirType' coerce #} -- Block argument can change their type
{#fun unsafe mlirValueDump { coerce `MlirValue' } -> `()' #}

{- OpOperand API
 - What happens after operation destruction
 - -}
{#fun unsafe mlirOpOperandIsNull { coerce `MlirOperation' } -> `Bool' #}
{#fun pure unsafe mlirOpOperandGetOwner { coerce `MlirOpOperand' } -> `MlirOperation' coerce #} -- Should be pure
{#fun pure unsafe mlirOpOperandGetOperandNumber { coerce `MlirOpOperand' } -> `Word' fromIntegral #}
{#fun unsafe mlirOpOperandGetNextUse { coerce `MlirOpOperand' } -> `MlirOpOperand' coerce #}

{- Type API
 - -}
{#fun unsafe mlirTypeParseGet__hswrap as mlirTypeParseGet { coerce `MlirContext', `String'& } -> `MlirType' coerce #} -- Should not be pure
{#fun unsafe mlirTypeGetContext { coerce `MlirType' } -> `MlirContext' coerce #}
{#fun unsafe mlirTypeGetTypeID { coerce `MlirType' } -> `MlirTypeID' coerce #}
{#fun unsafe mlirTypeGetDialect { coerce `MlirType' } -> `MlirDialect' coerce #}

mlirTypeIsNull :: MlirType -> Bool
mlirTypeIsNull (MlirType t) = t == nullPtr

{#fun pure unsafe mlirTypeEqual { coerce `MlirType', coerce `MlirType' } -> `Bool' #}
-- TODO: mlirTypePrint
{#fun unsafe mlirTypeDump { coerce `MlirType' } -> `()' #}

{- Attribute API
 - -}
{#fun unsafe mlirAttributeParseGet__hswrap as mlirAttributeParseGet { coerce `MlirContext', `String'& } -> `MlirAttribute' coerce #}
{#fun pure unsafe mlirAttributeGetContext { coerce `MlirAttribute' } -> `MlirContext' coerce #}
{#fun pure unsafe mlirAttributeGetType { coerce `MlirAttribute' } -> `MlirType' coerce #}
{#fun pure unsafe mlirAttributeGetTypeID { coerce `MlirAttribute' } -> `MlirTypeID' coerce #}
{#fun pure unsafe mlirAttributeGetDialect { coerce `MlirAttribute' } -> `MlirDialect' coerce #}
mlirAttributeIsNull :: MlirAttribute -> Bool
mlirAttributeIsNull (MlirAttribute attr) = attr == nullPtr
{#fun pure unsafe mlirAttributeEqual { coerce `MlirAttribute', coerce `MlirAttribute' } -> `Bool' #}
{#fun unsafe mlirAttributeDump { coerce `MlirAttribute' } -> `()' #}

mlirNamedAttributeGet :: MlirIdentifier -> MlirAttribute -> MlirNamedAttribute
mlirNamedAttributeGet = MlirNamedAttribute

{#fun pure unsafe mlirIdentifierGet__hswrap as mlirIdentifierGet { coerce `MlirContext', `String'& } -> `MlirIdentifier' coerce #}
{#fun pure unsafe mlirIdentifierGetContext { coerce `MlirIdentifier' } -> `MlirContext' coerce #}
{#fun pure unsafe mlirIdentifierEqual { coerce `MlirIdentifier', coerce `MlirIdentifier' } -> `Bool' #}
{#fun pure unsafe mlirIdentifierStr__hswrap as mlirIdentifierStr { coerce `MlirIdentifier', alloca- `String' peekStringRefPtr* } -> `()' #}

-- TODO: Move wrapping functions from cbit/* to this file
