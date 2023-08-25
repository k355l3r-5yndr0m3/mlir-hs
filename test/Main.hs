module Main (main) where
import MLIR
import qualified MLIR.Dialect.Func as Func
import MLIR.C.IR (mlirModuleGetOperation)

main :: IO ()
main = do
  b <- withContext $ do
    loadDialect_ Func.dialect 
    m <- moduleOp $ do
      Func._FuncOp (stringAttr "main")
             (typeAttr $ functionType [] [])
             Nothing Nothing Nothing
             $ do 
        bb0 <- addBlock []
        defBlock bb0 $ do
          _ <- Func._ConstantOp (flatSymbolRefAttr "main") (functionType [] [])
          return ()
        return ()
    dumpModule m
    bc <- operationWriteBytecode $ mlirModuleGetOperation m
    moduleDestroy m
    return bc
  print $ bytecodeSize b
