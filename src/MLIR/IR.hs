module MLIR.IR where
import MLIR.C.IR as C

import Control.Monad
import Data.Primitive.ByteArray
import System.IO
import Control.Exception (assert)
import Foreign (withArrayLen, withArray, castPtr)
import Foreign.C.Types (CIntPtr)
import Data.Primitive.ByteArray

newtype ContextM a = ContextM (Context -> IO a)
contextRunIO :: IO a -> ContextM a
contextRunIO a = ContextM (const a)

instance Functor ContextM where
  fmap f (ContextM a) = ContextM $ \ c -> fmap f (a c)
instance Applicative ContextM where
  pure a = ContextM $ \ _ -> return a
  (ContextM f) <*> (ContextM a) = ContextM $ \ c -> f c <*> a c
  liftA2 f (ContextM a) (ContextM b) = ContextM $ \ c -> f <$> a c <*> b c
instance Monad ContextM where
  (ContextM a) >>= f = ContextM $ \ c -> do 
    a' <- a c 
    let ContextM b' = f a'
    b' c

newtype RegionM a = RegionM (Context -> Region -> IO a)
regionRunIO :: IO a -> RegionM a
regionRunIO a = RegionM (\ _ _ -> a)

blockGet :: [AnyType] -> RegionM Block
blockGet inputs = RegionM $ \ c r -> do
  let loc = locationUnknownGet c
  inputs' <- forM inputs (`typeGet` c)
  withArrayLen inputs' $ \ numArgs args -> 
    withArray (replicate numArgs loc) $ \ loc' -> do 
      b <- blockCreate (fromIntegral numArgs) args loc'
      regionAppendOwnedBlock r b 
      return b

blockDef :: Block -> BlockM () -> RegionM ()
blockDef b (BlockM f) = RegionM $ \ c _ -> f c b

blockArg :: CIntPtr -> BlockM Value
blockArg i = BlockM $ const (`blockGetArgument` i)

instance Functor RegionM where
  fmap f (RegionM a) = RegionM $ \ c r -> fmap f (a c r)
instance Applicative RegionM where
  pure a = RegionM $ \ _ _ -> return a
  (RegionM f) <*> (RegionM a) = RegionM $ \ c r -> f c r <*> a c r
  liftA2 f (RegionM a) (RegionM b) = RegionM $ \ c r -> f <$> a c r <*> b c r
instance Monad RegionM where
  (RegionM a) >>= f = RegionM $ \ c r -> do 
    a' <- a c r
    let RegionM b' = f a'
    b' c r


newtype BlockM a = BlockM (Context -> Block -> IO a)
blockRunIO :: IO a -> BlockM a
blockRunIO a = BlockM (\ _ _ -> a)


instance Functor BlockM where
  fmap f (BlockM a) = BlockM $ \ c b -> fmap f (a c b)
instance Applicative BlockM where
  pure a = BlockM $ \ _ _ -> return a
  (BlockM f) <*> (BlockM a) = BlockM $ \ c b -> f c b <*> a c b
  liftA2 g (BlockM a) (BlockM f) = BlockM $ \ c b -> g <$> a c b <*> f c b
instance Monad BlockM where
  (BlockM a) >>= f = BlockM $ \ c b -> do 
    a' <- a c b
    let BlockM b' = f a'
    b' c b


runContextM :: ContextM a -> IO a
runContextM (ContextM f) = do 
  c <- contextCreate
  a <- f c 
  contextDestroy c
  return a

loadDialect :: DialectHandle -> ContextM Dialect
loadDialect handle = ContextM $ dialectHandleLoadDialect handle

loadDialect_ :: DialectHandle -> ContextM ()
loadDialect_ handle = void $ loadDialect handle

-- Attributes
newtype AnyAttr = AnyAttr (Context -> IO Attribute)
class AttrGet a where
  attrGet :: a -> Context -> IO Attribute
  
  toAnyAttr :: a -> AnyAttr
  toAnyAttr a = AnyAttr $ attrGet a

instance AttrGet AnyAttr where
  attrGet (AnyAttr a) = a
  toAnyAttr = id

-- Type
newtype AnyType = AnyType (Context -> IO Type)
class TypeGet a where
  typeGet :: a -> Context -> IO Type
  
  toAnyType :: a -> AnyType
  toAnyType a = AnyType $ typeGet a

instance TypeGet AnyType where
  typeGet (AnyType a) = a
  toAnyType = id


-- Module
moduleOp :: BlockM () -> ContextM Module
moduleOp (BlockM f) = ContextM $ \ c -> do
  let loc = locationUnknownGet c
  m <- moduleCreateEmpty loc
  f c (moduleGetBody m)
  return m

moduleDestroy :: Module -> ContextM ()
moduleDestroy m = ContextM $ const (C.moduleDestroy m)

moduleDump :: Module -> ContextM ()
moduleDump m = ContextM $ const $ C.operationDump (C.moduleGetOperation m)

moduleGetOperation :: Module -> Operation
moduleGetOperation = C.moduleGetOperation

-- ByteCode
newtype ByteCode = ByteCode ByteArray
writeByteCode :: Operation -> ContextM ByteCode 
writeByteCode operation = contextRunIO $ do 
  buffer0 <- newPinnedByteArray initialBufferSize  
  requiredSize <- C.writeByteCode operation initialBufferSize (castPtr $ mutableByteArrayContents buffer0) 
  if requiredSize > initialBufferSize then do 
    buffer1 <- newPinnedByteArray (fromIntegral requiredSize)
    _ <- C.writeByteCode operation requiredSize (castPtr $ mutableByteArrayContents buffer1)
    ByteCode <$> unsafeFreezeByteArray buffer1
  else 
    if requiredSize == initialBufferSize then
      ByteCode <$> unsafeFreezeByteArray buffer0
    else do
      shrinkMutableByteArray buffer0 (fromIntegral requiredSize)
      ByteCode <$> unsafeFreezeByteArray buffer0
  where initialBufferSize :: Num i => i
        initialBufferSize = 512

instance Show ByteCode where 
  show (ByteCode byteCode) = show byteCode

saveByteCode :: ByteCode -> FilePath -> IO ()
saveByteCode (ByteCode byteCode) filepath = assert (isByteArrayPinned byteCode) $ withBinaryFile filepath WriteMode (\ handle ->
  hPutBuf handle ptr size)
  where ptr  = byteArrayContents byteCode
        size = sizeofByteArray byteCode


