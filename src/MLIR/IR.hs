{-# OPTIONS_GHC -Wall #-}
module MLIR.IR 
( module MLIR.IR
-- TODO: Do not reexport all declarations from MLIR.FFI.IR 
-- some functions might not be safe. 
-- And some functions are not needed at high level.
, module MLIR.FFI.IR
) where
import MLIR.FFI.IR

import Control.Exception (assert)

import Data.Primitive
import Data.IORef

import System.IO

-- A pinned bytearray
type Bytecode = ByteArray

withMlirContext :: (IO MlirContext -> (MlirContext -> IO ()) -> a) -> a
withMlirContext bracket = bracket mlirContextCreate mlirContextDestroy

withMlirContextThreading :: Bool -> (IO MlirContext -> (MlirContext -> IO ()) -> a) -> a
withMlirContextThreading threadingEnabled bracket = bracket (mlirContextCreateWithThreading threadingEnabled) mlirContextDestroy

withMlirContextRegistry :: MlirDialectRegistry -> Bool -> (IO MlirContext -> (MlirContext -> IO ()) -> a) -> a
withMlirContextRegistry registry threadingEnabled bracket = bracket (mlirContextCreateWithRegistry registry threadingEnabled) mlirContextDestroy

-- Dialect registry backet
withMlirDialectRegistry :: (IO MlirDialectRegistry -> (MlirDialectRegistry -> IO ()) -> a) -> a
withMlirDialectRegistry bracket = bracket mlirDialectRegistryCreate mlirDialectRegistryDestroy

-- Module bracket
withMlirModule :: MlirLocation -> (IO MlirModule -> (MlirModule -> IO ()) -> a) -> a
withMlirModule location bracket = bracket (mlirModuleCreateEmpty location) mlirModuleDestroy

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
