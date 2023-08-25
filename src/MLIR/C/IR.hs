{-# LANGUAGE ForeignFunctionInterface, ViewPatterns #-}
module MLIR.C.IR where

import Control.Monad
import Data.Primitive

import Foreign
import Foreign.C

newtype Context = Context (Ptr ())
  deriving (Storable)
newtype Location = Location (Ptr ())
  deriving (Storable)
newtype Module = Module (Ptr ()) 
  deriving (Storable)
newtype Operation = Operation (Ptr ())
  deriving (Storable)
newtype Value = Value (Ptr ())
  deriving (Storable, Show)
newtype Type = Type (Ptr ())
  deriving (Storable)
newtype Identifier = Identifier (Ptr ())
  deriving (Storable)
newtype Attribute = Attribute (Ptr ())
  deriving (Storable)
newtype Block = Block (Ptr ())
  deriving (Storable)
newtype Region = Region (Ptr ())
  deriving (Storable)
newtype DialectHandle = DialectHandle (Ptr ())
  deriving (Storable)
newtype Dialect = Dialect (Ptr ()) 
  deriving (Storable)

-- Context
foreign import ccall unsafe "mlirContextCreate" mlirContextCreate :: IO Context
foreign import ccall unsafe "mlirContextDestroy" mlirContextDestroy :: Context -> IO ()

-- Location
foreign import ccall unsafe "mlirLocationUnknownGet" mlirLocationUnknownGet  :: Context -> IO Location

-- Module
foreign import ccall unsafe "mlirModuleCreateEmpty" mlirModuleCreateEmpty :: Location -> IO Module
foreign import ccall unsafe "mlirModuleDestroy" mlirModuleDestroy :: Module -> IO ()

foreign import ccall unsafe "mlirModuleGetBody" mlirModuleGetBody :: Module -> Block
foreign import ccall unsafe "mlirModuleGetOperation" mlirModuleGetOperation :: Module -> Operation

-- Region
foreign import ccall unsafe "mlirRegionCreate" mlirRegionCreate :: IO Region
foreign import ccall unsafe "mlirRegionDestroy" mlirRegionDestroy :: Region -> IO ()
foreign import ccall unsafe "mlirRegionAppendOwnedBlock" mlirRegionAppendOwnedBlock :: Region -> Block -> IO ()

-- Block
foreign import ccall unsafe "mlirBlockCreate" mlirBlockCreate' :: CIntPtr -> Ptr Type -> Ptr Location -> IO Block
mlirBlockCreate :: [(Type, Location)] -> IO Block
mlirBlockCreate (unzip -> (argTypes, argLocs)) = 
  withArrayLen argTypes $ \(fromIntegral -> numArgs) argTypes' ->
    withArray argLocs $ \argLocs' -> 
      mlirBlockCreate' numArgs argTypes' argLocs'

foreign import ccall unsafe "mlirBlockAppendOwnedOperation" mlirBlockAppendOwnedOperation :: Block -> Operation -> IO ()
foreign import ccall unsafe "mlirBlockGetArgument" mlirBlockGetArgument :: Block -> CIntPtr -> IO Value

-- NamedAttribute
data NamedAttribute = NamedAttribute Identifier Attribute
foreign import ccall unsafe "sizeof__MlirNamedAttribute" sizeof__NamedAttribute :: CSize
foreign import ccall unsafe "alignof__MlirNamedAttribute" alignof__NamedAttribute :: CSize
foreign import ccall unsafe "poke__MlirNamedAttribute" poke__NamedAttribute :: Identifier -> Attribute -> Ptr NamedAttribute -> IO ()
foreign import ccall unsafe "peek__MlirNamedAttribute__attribute" peek__NamedAttribute__attribute :: Ptr NamedAttribute -> IO Attribute
foreign import ccall unsafe "peek__MlirNamedAttribute__name" peek__NamedAttribute__name :: Ptr NamedAttribute -> IO Identifier
instance Storable NamedAttribute where
  sizeOf _ = fromIntegral sizeof__NamedAttribute
  alignment _ = fromIntegral alignof__NamedAttribute
  poke mem (NamedAttribute name attribute) = poke__NamedAttribute name attribute mem
  peek mem = NamedAttribute 
    <$> peek__NamedAttribute__name mem 
    <*> peek__NamedAttribute__attribute mem


-- Operation creation
foreign import ccall unsafe "hs__mlirOperationCreate" 
  mlirOperationCreate' :: Ptr CChar -> CSize 
                       -> Location 
                       -> CIntPtr -> Ptr Type 
                       -> CIntPtr -> Ptr Value 
                       -> CIntPtr -> Ptr Region 
                       -> CIntPtr -> Ptr Block 
                       -> CIntPtr -> Ptr NamedAttribute 
                       -> CBool 
                       -> IO Operation
mlirOperationCreate :: String -> Location -> [Type] -> [Value] -> [Region] -> [Block] -> [NamedAttribute] -> Bool -> IO Operation
mlirOperationCreate name loc results operands regions successors attributes enableResultTypeInference = 
  withCStringLen name $ \(namePtr, fromIntegral -> nameLength) -> 
    withArrayLen results $ \(fromIntegral -> nResults) resultsPtr -> 
      withArrayLen operands $ \(fromIntegral -> nOperands) operandsPtr -> 
        withArrayLen regions $ \(fromIntegral -> nRegions) regionsPtr -> 
          withArrayLen successors $ \(fromIntegral -> nSuccessor) successorsPtr -> 
            withArrayLen attributes $ \(fromIntegral -> nAttributes) attributesPtr -> 
              mlirOperationCreate' 
                namePtr nameLength 
                loc 
                nResults resultsPtr 
                nOperands operandsPtr 
                nRegions regionsPtr 
                nSuccessor successorsPtr 
                nAttributes attributesPtr 
                (if enableResultTypeInference then 1 else 0)

-- Operation
foreign import ccall unsafe "mlirOperationDump" mlirOperationDump :: Operation -> IO ()

foreign import ccall unsafe "mlirOperationDestroy" mlirOperationDestroy :: Operation -> IO ()
foreign import ccall unsafe "mlirOperationClone" mlirOperationClone :: Operation -> IO Operation
foreign import ccall unsafe "mlirOperationGetNumResults" mlirOperationGetNumResults :: Operation -> IO CIntPtr
foreign import ccall unsafe "mlirOperationGetResult" mlirOperationGetResult :: Operation -> CIntPtr -> IO Value
mlirOperationGetAllResults :: Operation -> IO [Value]
mlirOperationGetAllResults op = do 
  numResults <- mlirOperationGetNumResults op 
  forM [0..numResults - 1] $ mlirOperationGetResult op

newtype Bytecode = Bytecode ByteArray
foreign import ccall unsafe "hs__mlirOperationWriteBytecode" 
  mlirOperationWriteBytecode' :: Operation -> Ptr CSize -> IO (Ptr Word8)
-- making bytecode, expensive copy operation, TODO: Optimize
mlirOperationWriteBytecode :: Operation -> IO Bytecode
mlirOperationWriteBytecode op = alloca $ \sizePtr -> do 
  cBytecode  <- mlirOperationWriteBytecode' op sizePtr
  bcsize     <- fromIntegral <$> peek sizePtr
  hsBytecode <- newByteArray bcsize
  copyPtrToMutableByteArray hsBytecode 0 cBytecode bcsize
  free cBytecode 
  Bytecode <$> unsafeFreezeByteArray hsBytecode


foreign import ccall unsafe "mlirDialectHandleLoadDialect" mlirDialectHandleLoadDialect :: DialectHandle -> Context -> IO Dialect

foreign import ccall unsafe "hs__mlirIdentifierGet" mlirIdentifierGet' :: Context -> Ptr CChar -> CSize -> IO Identifier
mlirIdentifierGet :: Context -> String -> IO Identifier
mlirIdentifierGet ctx str = 
  withCStringLen str $ \(strPtr, fromIntegral -> strLength) -> 
    mlirIdentifierGet' ctx strPtr strLength


foreign import ccall unsafe "hs__mlirAttributeParseGet" 
  mlirAttributeParseGet' :: Context -> Ptr CChar -> CSize -> IO Attribute
mlirAttributeParseGet :: Context -> String -> IO Attribute
mlirAttributeParseGet ctx attr = withCStringLen attr $ \(attrPtr, fromIntegral -> attrLength) ->
  mlirAttributeParseGet' ctx attrPtr attrLength


