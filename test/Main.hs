{-# LANGUAGE TemplateHaskell #-}
module Main (main) where
import Control.Monad.IO.Class
import MLIR.Dialect.Func
import MLIR.IR
import MLIR.TH

import Control.Monad

main :: IO ()
main = do
  -- includedir <- llvmIncludePath
  -- tblgen <- llvmTablegen [includedir] (includedir ++ "/mlir/Dialect/Func/IR/FuncOps.td")
  return ()

$(generateMlirDialect "mlir/Dialect/Func/IR/FuncOps.td")


