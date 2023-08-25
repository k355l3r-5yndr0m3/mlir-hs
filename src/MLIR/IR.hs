module MLIR.IR where

import MLIR.C.IR hiding (Type, Attribute)
import qualified MLIR.C.IR as C (Type, Attribute)

import Foreign.C (CIntPtr)
import Data.Primitive (sizeofByteArray)

newtype Attribute = Attribute { getAttribute :: Context -> IO C.Attribute }
(<#=) :: String -> Attribute -> Context -> IO NamedAttribute
name <#= attr = \ctx -> NamedAttribute <$> mlirIdentifierGet ctx name <*> getAttribute attr ctx

(<?=) :: String -> Maybe Attribute -> Maybe (Context -> IO NamedAttribute)
_    <?= Nothing     = Nothing
name <?= (Just attr) = Just (name <#= attr) 


-- newtype Location = Location { getLocation :: Context -> IO C.Location}
newtype Type = Type { getType :: Context -> IO C.Type }

newtype BlockM a = BlockM (Context -> Block -> IO a)
blockMIO :: IO a -> BlockM a
blockMIO m = BlockM $ \ _ _ -> m
instance Functor BlockM where
  fmap f (BlockM ma) = BlockM (\c b -> fmap f $ ma c b)
instance Applicative BlockM where
  pure a = BlockM (\_ _ -> return a)
  (BlockM mf) <*> (BlockM ma) = BlockM $ \c b -> do 
    f <- mf c b
    a <- ma c b
    return $ f a
instance Monad BlockM where
  (BlockM ma) >>= f = BlockM $ \c b -> do 
    a <- ma c b
    let BlockM mb = f a
    mb c b
blkArg :: CIntPtr -> BlockM Value
blkArg idx = BlockM $ \_ blk -> 
  mlirBlockGetArgument blk idx

newtype RegionM a = RegionM (Context -> Region -> IO a)
regionMIO :: IO a -> RegionM a 
regionMIO m = RegionM $ \ _ _ -> m
instance Functor RegionM where
  fmap f (RegionM ma) = RegionM (\c b -> fmap f $ ma c b)
instance Applicative RegionM where
  pure a = RegionM (\_ _ -> return a)
  (RegionM mf) <*> (RegionM ma) = RegionM $ \c b -> do 
    f <- mf c b
    a <- ma c b
    return $ f a
instance Monad RegionM where
  (RegionM ma) >>= f = RegionM $ \c b -> do 
    a <- ma c b
    let RegionM mb = f a
    mb c b

newRegion :: RegionM a -> Context -> IO Region
newRegion (RegionM rgn) ctx = do
  rgnPtr <- mlirRegionCreate 
  _ <- rgn ctx rgnPtr 
  return rgnPtr

-- Is there a better way
addBlock :: [Type] -> RegionM Block
addBlock args = RegionM $ \ctx rgn -> do 
  args' <- sequence $ getType <$> args <*> [ctx]
  loc   <- mlirLocationUnknownGet ctx
  blk   <- mlirBlockCreate $ zip args' $ repeat loc
  mlirRegionAppendOwnedBlock rgn blk
  return blk

defBlock :: Block -> BlockM a -> RegionM ()
defBlock blk (BlockM mkBlk) = RegionM $ \ctx _ -> do -- TODO: Beautify this
  _ <- mkBlk ctx blk
  return ()


newtype ContextM a = ContextM (Context -> IO a)
instance Functor ContextM where
  fmap f (ContextM ma) = ContextM $ \c -> fmap f $ ma c
instance Applicative ContextM where
  pure a = ContextM $ \_ -> return a
  (ContextM mf) <*> (ContextM ma) = ContextM $ \c -> do
    f <- mf c 
    a <- ma c
    return $ f a
instance Monad ContextM where
  (ContextM ma) >>= f = ContextM $ \c -> do 
    a <- ma c
    let ContextM mb = f a
    mb c

moduleOp :: BlockM a -> ContextM Module
moduleOp (BlockM blk) = ContextM $ \ctx -> do 
  m <- mlirLocationUnknownGet ctx >>= mlirModuleCreateEmpty
  _ <- blk ctx $ mlirModuleGetBody m
  return m

moduleDestroy :: Module -> ContextM ()
moduleDestroy m = ContextM $ \_ -> 
  mlirModuleDestroy m

withContext :: ContextM a -> IO a
withContext (ContextM ma) = do 
  ctx <- mlirContextCreate
  a   <- ma ctx
  mlirContextDestroy ctx
  return a

loadDialect :: DialectHandle -> ContextM Dialect
loadDialect handle = ContextM $ \ctx -> 
  mlirDialectHandleLoadDialect handle ctx

loadDialect_ :: DialectHandle -> ContextM ()
loadDialect_ handle = ContextM $ \ctx ->
  mlirDialectHandleLoadDialect handle ctx >> return ()

dumpModule :: Module -> ContextM ()
dumpModule m = ContextM $ \_ -> mlirOperationDump $ mlirModuleGetOperation m

operationWriteBytecode :: Operation -> ContextM Bytecode
operationWriteBytecode op = ContextM $ \_ ->
  mlirOperationWriteBytecode op

moduleWriteBytecode :: Module -> ContextM Bytecode 
moduleWriteBytecode m = operationWriteBytecode $ mlirModuleGetOperation m

bytecodeSize :: Bytecode -> Int
bytecodeSize (Bytecode bc) = sizeofByteArray bc


