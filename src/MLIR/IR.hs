{-# OPTIONS_GHC -Wall #-}
module MLIR.IR 
( module MLIR.IR
, module MLIR.FFI.IR 
) where
import MLIR.FFI.IR

import Control.Exception
import Control.Monad

import Data.Primitive
import Data.IORef

import System.IO


-- Functionalities:
--   + Constructing IR
--   + Inspecting IR
--   + Modifying IR (Either by reconstructing IR or modifying inplace)
-- Reconstruction is definately possible (inplace? might not be)
-- One massive problem with mlir is its ownership model
-- Some object is owned by the user (haskell runtime must free it)
-- Some object is owned by Mlir* (most of the time it is MlirContext, and it must not be freed by the runtime)
-- Object can switch between the two

-- A pinned bytearray
type Bytecode = ByteArray

withMlirContext :: (MlirContext -> IO a) -> IO a
withMlirContext = bracket mlirContextCreate mlirContextDestroy

withMlirContextThreading :: Bool -> (MlirContext -> IO a) -> IO a
withMlirContextThreading threadingEnabled = bracket (mlirContextCreateWithThreading threadingEnabled) mlirContextDestroy

withMlirContextRegistry :: MlirDialectRegistry -> Bool -> (MlirContext -> IO a) -> IO a
withMlirContextRegistry registry threadingEnabled = bracket (mlirContextCreateWithRegistry registry threadingEnabled) mlirContextDestroy

-- Dialect registry backet
withMlirDialectRegistry :: (MlirDialectRegistry -> IO a) -> IO a
withMlirDialectRegistry = bracket mlirDialectRegistryCreate mlirDialectRegistryDestroy

-- Module bracket
withMlirModule :: MlirLocation -> (MlirModule -> IO a) -> IO a
withMlirModule location = bracket (mlirModuleCreateEmpty location) mlirModuleDestroy

mlirModuleEmitBytecode :: MlirModule -> IO Bytecode
mlirModuleEmitBytecode = mlirOperationEmitBytecode . mlirModuleGetOperation

mlirModuleDump :: MlirModule -> IO ()
mlirModuleDump = mlirOperationDump . mlirModuleGetOperation

-- Operation bracket (always insert into the block)
mlirBlockAddOperation :: MlirBlock -> String -> MlirLocation -> [MlirType] -> [MlirValue] -> [MlirRegion -> IO ()] -> [MlirBlock] -> [MlirNamedAttribute] -> Bool -> IO (MlirOperation, [MlirValue])
mlirBlockAddOperation block opname location results operands regions_ successors attributes inference = do
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

blockAddOperation_ :: MlirBlock -> String -> MlirLocation -> [MlirType] -> [MlirValue] -> [MlirRegion -> IO ()] -> [MlirBlock] -> [MlirNamedAttribute] -> Bool -> IO [MlirValue]
blockAddOperation_ block opname location results operands regions successors attributes inference =
  snd <$> mlirBlockAddOperation block opname location results operands regions successors attributes inference

mlirOperationEmitBytecode :: MlirOperation -> IO Bytecode
mlirOperationEmitBytecode operation = do
  sizeRef <- newIORef 0
  let sizeCallback _ len =
        modifyIORef sizeRef (+len)
  mlirOperationWriteBytecode operation sizeCallback
  size <- readIORef sizeRef
  bytecode <- newPinnedByteArray size
  
  writeIORef sizeRef 0
  let writeCallback str len = do
        offset <- readIORef sizeRef
        modifyIORef sizeRef (+len)
        copyPtrToMutableByteArray bytecode offset str len
  mlirOperationWriteBytecode operation writeCallback
  unsafeFreezeByteArray bytecode

-- Blocks
mlirRegionAddBlock :: MlirRegion -> [(MlirType, MlirLocation)] -> IO MlirBlock
mlirRegionAddBlock region arguments = do
  block <- mlirBlockCreate arguments
  mlirRegionAppendOwnedBlock region block
  return block

-- Utilities (MOVE SOMEWHERE ELSE)
writeBytecodeToFile :: Bytecode -> FilePath -> IO ()
writeBytecodeToFile bytecode filepath = assert (isByteArrayPinned bytecode) $ withBinaryFile filepath WriteMode (\ fhandle ->
  hPutBuf fhandle (byteArrayContents bytecode) (sizeofByteArray bytecode))
