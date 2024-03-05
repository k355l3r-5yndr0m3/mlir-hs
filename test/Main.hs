{-# LANGUAGE TemplateHaskell #-}
module Main (main) where
import Control.Monad.IO.Class
import MLIR.IR
import MLIR.TH

import Control.Monad

main :: IO ()
main = do
  -- includedir <- llvmIncludePath
  -- tblgen <- llvmTablegen [includedir] (includedir ++ "/mlir/Dialect/Func/IR/FuncOps.td")
  return ()

$(generateMlirDialect "mlir/Dialect/Func/IR/FuncOps.td")
-- $(generateMlirDialect "mlir/Dialect/OpenMP/OpenMPOps.td")
--- $(generateMlirDialect "mlir/Dialect/MLProgram/IR/MLProgramOps.td")

