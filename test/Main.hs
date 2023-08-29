{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import MLIR

import qualified MLIR.Dialect.Func as Func

import qualified MLIR.Dialect.Arith as Arith
import qualified MLIR.Dialect.Arith.Attributes as Arith

main :: IO ()
main = do 
  byteCode <- runContextM $ do 
    loadDialect_ Func.dialect
    loadDialect_ Arith.dialect
    m <- moduleOp $ do 
      Func._FuncOp "main"
                   (TypeAttr $ FunctionType [] [])
                   Nothing Nothing Nothing $ do 
        return ()
      Func._FuncOp "test"
                   (TypeAttr $ FunctionType [toAnyType SI32, toAnyType SI32] [toAnyType SI32])
                   Nothing Nothing Nothing $ do 
        bb0 <- blockGet [toAnyType SI32, toAnyType SI32]
        blockDef bb0 $ do 
          _0 <- blockArg 0 
          _1 <- blockArg 1 
          _2 <- Arith._AddIOp _0 _1 $ toAnyType SI32
          Func._ReturnOp [_2]
        return ()
      return ()
    moduleDump m
    byteCode <- writeByteCode $ moduleGetOperation m
    moduleDestroy m
    return byteCode 
  saveByteCode byteCode "test.mlir"
