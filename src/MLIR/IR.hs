{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE  ViewPatterns #-}
module MLIR.IR where
import MLIR.FFI.Finalizers
import MLIR.FFI.IR

import Control.Monad
import Control.Monad.Reader
import Control.Exception

import Data.Coerce
import Data.Primitive
import Data.Void
import Data.IORef

import Foreign hiding (void)

import GHC.ForeignPtr

-- Functionalities:
--   + Constructing IR
--   + Inspecting IR
--   + Modifying IR (Either by reconstructing IR or modifying inplace)
-- Reconstruction is definately possible (inplace? might not be)
-- One massive problem with mlir is its ownership model
-- Some object is owned by the user (haskell runtime must free it)
-- Some object is owned by Mlir* (most of the time it is MlirContext, and it must not be freed by the runtime)
-- Object can switch between the two

-- So using foreignptr is a pain
-- bracket pattern limit the scope of objects but this seem like the only via way to keep my sanity
-- Context bracket
type Bytecode = ByteArray

type Context = MlirContext
type Dialect = MlirDialect
type DialectHandle = MlirDialectHandle
type DialectRegistry = MlirDialectRegistry
type Location = MlirLocation
type Module = MlirModule
type Block = MlirBlock
type Value = MlirValue
type Type = MlirType
type Region = MlirRegion
type NamedAttribute = MlirNamedAttribute
type Operation = MlirOperation
type Attribute = MlirAttribute

withContext :: (Context -> IO a) -> IO a
withContext = bracket mlirContextCreate mlirContextDestroy

withContextThreading :: Bool -> (Context -> IO a) -> IO a
withContextThreading threadingEnabled = bracket (mlirContextCreateWithThreading threadingEnabled) mlirContextDestroy

withContextRegistry :: DialectRegistry -> Bool -> (Context -> IO a) -> IO a
withContextRegistry registry threadingEnabled = bracket (mlirContextCreateWithRegistry registry threadingEnabled) mlirContextDestroy

-- Dialect handle
dialectHandleLoadDialect :: DialectHandle -> Context -> IO Dialect
dialectHandleLoadDialect = mlirDialectHandleLoadDialect

dialectHandleRegisterDialect :: DialectHandle -> Context -> IO ()
dialectHandleRegisterDialect = mlirDialectHandleRegisterDialect

dialectHandleInsertDialect :: MlirDialectHandle -> MlirDialectRegistry -> IO ()
dialectHandleInsertDialect = mlirDialectHandleInsertDialect

-- Dialect registry backet
withDialectRegistry :: (DialectRegistry -> IO a) -> IO a
withDialectRegistry = bracket mlirDialectRegistryCreate mlirDialectRegistryDestroy

-- Module bracket
withModule :: Location -> (Module -> IO a) -> IO a
withModule location = bracket (mlirModuleCreateEmpty location) mlirModuleDestroy

moduleEmitBytecode :: Module -> IO Bytecode
moduleEmitBytecode = operationEmitBytecode . mlirModuleGetOperation

-- Operation bracket (always insert into the block)
blockAddOperation :: Block -> String -> Location -> [Type] -> [Value] -> [Region -> IO ()] -> [Block] -> [NamedAttribute] -> Bool -> IO (Operation, [Value])
blockAddOperation block opname location results operands regions_ successors attributes inference = do
  regions   <- mapM (\ rbody -> do r <- mlirRegionCreate; rbody r; return r) regions_
  operation <- mlirOperationCreate opname
                                   location
                                   results
                                   operands
                                   regions
                                   successors
                                   attributes
                                   inference
  mlirBlockAppendOwnedOperation block operation
  return (operation, map (mlirOperationGetResult operation) $ take (mlirOperationGetNumResults operation) [0..])

blockAddOperation_ :: Block -> String -> Location -> [Type] -> [Value] -> [Region -> IO ()] -> [Block] -> [NamedAttribute] -> Bool -> IO [Value]
blockAddOperation_ block opname location results operands regions successors attributes inference =
  snd <$> blockAddOperation block opname location results operands regions successors attributes inference

operationEmitBytecode :: Operation -> IO Bytecode
operationEmitBytecode operation = do
  sizeRef <- newIORef 0
  let sizeCallback _ len =
        modifyIORef sizeRef (+len)
  mlirOperationWriteBytecode operation sizeCallback
  size <- readIORef sizeRef
  bytecode <- newByteArray size
  
  writeIORef sizeRef 0
  let writeCallback str len = do
        offset <- readIORef sizeRef
        modifyIORef sizeRef (+len)
        copyPtrToMutableByteArray bytecode offset str len
  mlirOperationWriteBytecode operation writeCallback
  unsafeFreezeByteArray bytecode

-- Blocks
regionAddBlock :: Region -> [(Type, Location)] -> IO Block
regionAddBlock region arguments = do
  block <- mlirBlockCreate arguments
  mlirRegionAppendOwnedBlock region block
  return block
