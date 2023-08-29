{-# LANGUAGE ViewPatterns #-}
module MLIR.AutogenUtils (
  (<#=), (<?=), (?:)
, operationCreate
, operationGetAllResults
, operationGetResult
, runRegionM
, locationUnknownGet
, blockAppendOwnedOperation
) where
import qualified MLIR.C.IR as C (operationCreate)
import MLIR.C.IR hiding (operationCreate)
import MLIR.IR

import Control.Monad

import Foreign.C
import Foreign

(<#=) :: AttrGet a => String -> a -> Context -> IO NamedAttribute
name <#= (attrGet -> attr) = \ c -> do
  name' <- withCStringLen name $ \ (str, fromIntegral -> strLen) -> identifierGet c str strLen
  attr' <- attr c
  return $ NamedAttribute name' attr'

(<?=) :: AttrGet a => String -> Maybe a -> Maybe (Context -> IO NamedAttribute)
_    <?= Nothing   = Nothing
name <?= Just attr = Just $ name <#= attr

operationCreate :: String -> Location -> [Type] -> [Value] -> [Region] -> [Block] -> [NamedAttribute] -> IO Operation
operationCreate opname loc results arguments regions successors attributes = 
  withCStringLen opname $ \ (opname', fromIntegral -> opnameLen) -> 
    withArrayLen results $ \ (fromIntegral -> numResults) results' -> 
      withArrayLen arguments $ \ (fromIntegral -> numArgs) args' -> 
        withArrayLen regions $ \ (fromIntegral -> numRegs) regs' ->
          withArrayLen successors $ \ (fromIntegral -> numSuccs) succs' -> 
            withArrayLen attributes $ \ (fromIntegral -> numAttrs) attrs' -> 
              C.operationCreate opname' opnameLen loc
                                numResults results' 
                                numArgs args'
                                numRegs regs' 
                                numSuccs succs'
                                numAttrs attrs'
                                0

(?:) :: Maybe a -> [a] -> [a]
Just a  ?: as = a:as
Nothing ?: as = as
infixr 5 ?:

operationGetAllResults :: Operation -> IO [Value]
operationGetAllResults operation = do 
  numResults <- operationGetNumResults operation
  forM [0..numResults - 1] (operationGetResult operation)


runRegionM :: RegionM () -> Context -> IO Region
runRegionM (RegionM f) c = do 
  r <- regionCreate 
  f c r 
  return r

