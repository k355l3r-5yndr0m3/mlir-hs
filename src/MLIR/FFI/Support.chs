{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
module MLIR.FFI.Support where
#include <mlir-c/Support.h>
import Control.Monad

import Foreign
import Foreign.C

import Data.Coerce
import Data.Void

data MlirStringRef = MlirStringRef
  { mlirStringRefData   :: !CString
  , mlirStringRefLength :: !CSize
  }
type MlirStringRefPtr = Ptr MlirStringRef
{#pointer *MlirStringRef as MlirStringRefPtr nocode #}
instance Storable MlirStringRef where
  sizeOf _ = {#sizeof MlirStringRef#}
  alignment _ = {#alignof MlirStringRef#}

  peek p = MlirStringRef <$> {#get MlirStringRef->data#} p
                         <*> fmap coerce ({#get MlirStringRef->length#} p)
  poke p (MlirStringRef {..}) = {#set MlirStringRef->data#} p mlirStringRefData >>
                                {#set MlirStringRef->length#} p (coerce mlirStringRefLength)

peekStringRef :: MlirStringRef -> IO String
peekStringRef ref = peekCStringLen (mlirStringRefData ref, fromIntegral $ mlirStringRefLength ref)

peekStringRefPtr :: Ptr MlirStringRef -> IO String
peekStringRefPtr = peekStringRef <=< peek

stringRef :: (Ptr MlirStringRef -> IO a) -> IO (MlirStringRef, a)
stringRef proc = alloca (\ ref' -> do 
  ret <- proc ref'
  (, ret) <$> peek ref')

stringRef_ :: (Ptr MlirStringRef -> IO a) -> IO MlirStringRef
stringRef_ proc = alloca (\ ref' -> proc ref' >> peek ref')

newtype MlirTypeID = MlirTypeID (Ptr Void)

type MlirStringCallback = Ptr Word8 -> Int -> IO ()
foreign export ccall mlirStringCallback__cwrap :: CString -> CSize -> Ptr () -> IO ()
mlirStringCallback__cwrap :: CString -> CSize -> Ptr () -> IO ()
mlirStringCallback__cwrap strRefData strRefLength callbackPtr = do
  callback :: MlirStringCallback <- deRefStablePtr $ castPtrToStablePtr callbackPtr
  callback (castPtr strRefData :: Ptr Word8) (fromIntegral strRefLength :: Int)

-- NOTE: "&" must not be added because then template-haskell will freak out
-- mlirStringCallback__hswrap return a function pointer
foreign import ccall "mlirStringCallback__hswrap" mlirStringCallbackFunPtr :: FunPtr (MlirStringRef -> Ptr Void -> IO ())
-- mlirStringCallbackFunPtr :: FunPtr (MlirStringRef -> Ptr Void -> IO ())
-- mlirStringCallbackFunPtr = undefined

{-# INLINE withMlirStringCallback #-}
withMlirStringCallback :: MlirStringCallback -> ((FunPtr (Ptr () {- c2hs think all structs are ptr () -} -> Ptr () -> IO ()), Ptr () {- StablePtr MlirStringCallback -}) -> IO a) -> IO a
withMlirStringCallback callback proc = do
  ptr <- newStablePtr callback
  result <- proc (castFunPtr mlirStringCallbackFunPtr, castStablePtrToPtr ptr)
  freeStablePtr ptr
  return result
