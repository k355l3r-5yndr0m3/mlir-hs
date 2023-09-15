{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}
module MLIR.C.IR where

import Foreign
import Foreign.C
import GHC.Exts (MutableByteArray#, RealWorld)

newtype Context = Context (Ptr ())
newtype DialectHandle = DialectHandle (Ptr ())
newtype Dialect = Dialect (Ptr ())
newtype Location = Location (Ptr ()) deriving Storable
newtype Module = Module (Ptr ())
newtype Block = Block (Ptr ()) deriving Storable
newtype Type = Type (Ptr ()) deriving Storable
newtype Value = Value (Ptr ()) deriving Storable
newtype Region = Region (Ptr ()) deriving Storable
newtype Operation = Operation (Ptr ())
newtype Identifier = Identifier (Ptr ())
newtype Attribute = Attribute (Ptr ()) deriving Storable

-- Context 
foreign import ccall unsafe "mlirContextCreate"
  contextCreate :: IO Context
foreign import ccall unsafe "mlirContextDestroy"
  contextDestroy :: Context -> IO ()

-- Location
foreign import ccall unsafe "mlirLocationUnknownGet"
  locationUnknownGet :: Context -> Location

-- Dialect handle
foreign import ccall unsafe "mlirDialectHandleLoadDialect"
  dialectHandleLoadDialect :: DialectHandle -> Context -> IO Dialect

-- Module 
foreign import ccall unsafe "mlirModuleCreateEmpty"
  moduleCreateEmpty :: Location -> IO Module
foreign import ccall unsafe "mlirModuleDestroy"
  moduleDestroy :: Module -> IO ()

foreign import ccall unsafe "mlirModuleGetBody"
  moduleGetBody :: Module -> Block

foreign import ccall unsafe "mlirModuleGetOperation"
  moduleGetOperation :: Module -> Operation

-- Operation
data NamedAttribute = NamedAttribute Identifier Attribute
foreign import ccall unsafe "sizeof__MlirNamedAttribute" sizeof__MlirNamedAttribute :: CSize
foreign import ccall unsafe "alignof__MlirNamedAttribute" alignof__MlirNamedAttribute :: CSize
foreign import ccall unsafe "poke__MlirNamedAttribute" poke__MlirNamedAttribute :: Identifier -> Attribute -> Ptr NamedAttribute -> IO ()
foreign import ccall unsafe "peek__MlirNamedAttribute__attribute" peek__MlirNamedAttribute__attribute :: Ptr NamedAttribute -> IO Attribute
foreign import ccall unsafe "peek__MlirNamedAttribute__name" peek__MlirNamedAttribute__name :: Ptr NamedAttribute -> IO Identifier
instance Storable NamedAttribute where
  sizeOf _ = fromIntegral sizeof__MlirNamedAttribute
  alignment _ = fromIntegral alignof__MlirNamedAttribute
  peek ptr = NamedAttribute <$> peek__MlirNamedAttribute__name ptr <*> peek__MlirNamedAttribute__attribute ptr
  poke ptr (NamedAttribute name attribute) = poke__MlirNamedAttribute name attribute ptr 


foreign import ccall unsafe "hs__mlirOperationCreate"
  operationCreate :: CString -> CSize -> Location -> 
                     CIntPtr -> Ptr Type -> 
                     CIntPtr -> Ptr Value ->
                     CIntPtr -> Ptr Region -> 
                     CIntPtr -> Ptr Block ->
                     CIntPtr -> Ptr NamedAttribute ->
                     CBool -> IO Operation
foreign import ccall unsafe "mlirOperationDestroy"
  operationDestroy :: Operation -> IO ()

foreign import ccall unsafe "mlirOperationDump"
  operationDump :: Operation -> IO ()

foreign import ccall unsafe "mlirOperationVerify"
  operationVerify :: Operation -> IO CBool

foreign import ccall unsafe "mlirOperationGetNumResults"
  operationGetNumResults :: Operation -> IO CIntPtr

foreign import ccall unsafe "mlirOperationGetResult"
  operationGetResult :: Operation -> CIntPtr -> IO Value




-- Region
foreign import ccall unsafe "mlirRegionCreate"
  regionCreate :: IO Region
foreign import ccall unsafe "mlirRegionDestroy"
  regionDestroy :: Region -> IO ()

foreign import ccall unsafe "mlirRegionAppendOwnedBlock"
  regionAppendOwnedBlock :: Region -> Block -> IO ()
-- Block
foreign import ccall unsafe "mlirBlockCreate"
  blockCreate :: CIntPtr -> Ptr Type -> Ptr Location -> IO Block
foreign import ccall unsafe "mlirBlockDestroy"
  blockDestroy :: Block -> IO ()

foreign import ccall unsafe "mlirBlockAppendOwnedOperation"
  blockAppendOwnedOperation :: Block -> Operation -> IO ()
-- Consider removing some of the IO because 
-- block is immutable unless some more functions 
-- are imported
foreign import ccall unsafe "mlirBlockGetArgument"
  blockGetArgument :: Block -> CIntPtr -> IO Value 

-- Identifier
foreign import ccall unsafe "hs__mlirIdentifierGet"
  identifierGet :: Context -> CString -> CSize -> IO Identifier


-- User defined
-- foreign import ccall unsafe "write_bytecode"
--   writeByteCode :: Operation -> CSize -> CSize -> MutableByteArray# RealWorld -> IO CSize
foreign import ccall unsafe "hs__mlirOperationWriteBytecode"
  writeByteCode :: Operation -> CSize -> Ptr Char -> IO CSize

