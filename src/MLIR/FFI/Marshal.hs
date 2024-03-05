module MLIR.FFI.Marshal 
( module Data.Primitive
, module MLIR.FFI.Marshal
) where
import Foreign
import MLIR.FFI.Support
import Data.Void
import Data.Coerce
import Data.Primitive
import Control.Exception (assert)

{-# INLINE marshalArrayLen #-}
marshalArrayLen :: (Storable b, Num c) => (a -> b) -> (c -> Ptr b -> d) -> [a] -> (d -> IO e) -> IO e 
marshalArrayLen from prep list proc = withArrayLen (map from list) (\ len ptr -> proc $ prep (fromIntegral len) ptr)

{-# INLINE marshalIntegralArrayLen #-}
marshalIntegralArrayLen :: (Integral a, Storable b, Num b, Num c) => [a] -> ((c, Ptr b) -> IO d) -> IO d
marshalIntegralArrayLen = marshalArrayLen fromIntegral (,)

{-# INLINE marshalStorableArrayLen #-}
marshalStorableArrayLen :: (Storable a, Num b) => [a] -> ((b, Ptr a) -> IO c) -> IO c
marshalStorableArrayLen = marshalArrayLen id (,)

{-# INLINE marshalBoolArrayLen #-}
marshalBoolArrayLen :: (Num a, Storable a, Num b) => [Bool] -> ((b, Ptr a) -> IO c) -> IO c
marshalBoolArrayLen = marshalArrayLen fromBool (,)

{-# INLINE marshalFloatArrayLen #-}
marshalFloatArrayLen :: (Real a, Fractional b, Storable b, Num c) => [a] -> ((c, Ptr b) -> IO d) -> IO d
marshalFloatArrayLen = marshalArrayLen realToFrac (,)


{-# INLINE marshalPrimArray #-}
marshalPrimArray :: (Prim a, Num b, Coercible a c) => PrimArray a -> ((b, Ptr c) -> IO d) -> IO d
marshalPrimArray primarray procedure = assert (isPrimArrayPinned primarray) $
  procedure (fromIntegral $ sizeofPrimArray primarray, castPtr $ primArrayContents primarray)

{-# INLINE marshalByteArray #-}
marshalByteArray :: (Coercible Word8 a, Num b) => ByteArray -> ((b, Ptr a) -> IO c) -> IO c
marshalByteArray bytearray procedure = assert (isByteArrayPinned bytearray) $
  procedure (fromIntegral $ sizeofByteArray bytearray, castPtr $ byteArrayContents bytearray)

{-# INLINE marshalByteArraySwap #-}
marshalByteArraySwap :: Num b => ByteArray -> ((Ptr a, b) -> IO c) -> IO c
marshalByteArraySwap bytearray procedure = assert (isByteArrayPinned bytearray) $
  procedure (castPtr $ byteArrayContents bytearray, fromIntegral $ sizeofByteArray bytearray)

{-# INLINE marshalStorableArrayLenCast #-}
marshalStorableArrayLenCast :: (Storable a, Num b) => [a] -> ((b, Ptr d) -> IO c) -> IO c
marshalStorableArrayLenCast = marshalArrayLen id $ \ a b -> (a, castPtr b)

{-# INLINE newStablePtrAndCast #-}
newStablePtrAndCast :: a -> IO (Ptr b)
newStablePtrAndCast = fmap (castPtr . castStablePtrToPtr) . newStablePtr

{-# INLINE castAndFreeStablePtr #-}
castAndFreeStablePtr :: Ptr b -> IO ()
castAndFreeStablePtr = freeStablePtr . castPtrToStablePtr . castPtr

{-# INLINE withStablePtr #-}
withStablePtr :: a -> (Ptr () -> IO b) -> IO b
withStablePtr hsobj proc = do
  ptr <- newStablePtr hsobj
  result <- proc $ castStablePtrToPtr ptr
  freeStablePtr ptr
  return result

