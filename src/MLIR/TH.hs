{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module MLIR.TH where
import MLIR.IR as IR
import Tablegen

import Data.Aeson
import Data.String (IsString(fromString))
import Data.Either (partitionEithers)
import Data.Text   (unpack)

import System.Process (shell, readCreateProcess)
import System.FilePath

import Language.Haskell.TH

import Data.List   as L

import qualified Data.Vector as V

llvmIncludePath :: IO FilePath
llvmIncludePath = init <$> readCreateProcess (shell "llvm-config --includedir ") ""


llvmTablegen :: [FilePath] -> FilePath -> IO Tablegen
llvmTablegen includedirs source = do
  jsonString <- readCreateProcess (shell $ "llvm-tblgen --dump-json " ++ L.unwords ["-I"++dir | dir <- includedirs] ++ " " ++ source) ""
  case eitherDecode (fromString jsonString) of
    Left errmsg -> fail errmsg
    Right tblgen -> return tblgen

generateOperation' :: Tablegen -> Op -> DecsQ
generateOperation' tblgen operation = do
  args       <- maybeFail "args"       $ fmap V.toList $ mapM getDagValueName $ dagArgs $ opArguments  operation
  results    <- maybeFail "results"    $ fmap V.toList $ mapM getDagValue     $ dagArgs $ opResults    operation
  regions    <- maybeFail "regions"    $ fmap (fmap (defInstanceof @VariadicRegion tblgen) . V.toList) $ mapM getDagValue $ dagArgs $ opRegions    operation
  successors <- maybeFail "successors" $ fmap (fmap (defInstanceof @VariadicSuccessor tblgen) . V.toList) $ mapM getDagValue $ dagArgs $ opSuccessors operation
  let (operands, attributes) = partitionEithers $ fmap (\ (v, n) -> 
                                                          if defInstanceof @TypeConstraint tblgen v then
                                                            Left v
                                                          else
                                                            Right (v, n)) args
      builderType = 
        let resultType | null results = [t| IO IR.Operation |]
                       | otherwise    = [t| IO $(foldl (\ t r ->
                                                          if defInstanceof @Variadic tblgen r then
                                                            [t| $t [IR.Value] |]
                                                          else
                                                            [t| $t  IR.Value  |]) [t| $(tupleT $ length results + 1) IR.Operation |] results) |]
            succs      = foldr (\ r t -> 
                                  if r then
                                    [t| [IR.Block] -> $t |]
                                  else
                                    [t|  IR.Block  -> $t |]) resultType successors
            regs       = foldr (\ r t -> 
                                  if r then
                                    [t| [IR.Region -> IO ()] -> $t |]
                                  else
                                    [t| (Region -> IO ()) -> $t |]) succs regions
            attrs      = foldr (\ _ t -> [t| IR.Attribute -> $t |]) regs attributes
            opers      = foldr (\ v t -> [t| $(if defInstanceof @VariadicOfVariadic tblgen v then
                                                 [t| [[IR.Value]] |]
                                               else if defInstanceof @Variadic tblgen v then
                                                 [t| [IR.Value] |]
                                               else
                                                 [t| IR.Value |]) -> $t |]) attrs operands
        in  [t| IR.Context -> IR.Block -> $opers |]
  operandVars   <- mapM (sequence . (, newName "operand")) operands
  attributeVars <- mapM (\ (v, n) -> (v, n,) <$> newName "attribute") attributes
  regionVars    <- mapM (sequence . (, newName "region")) regions
  successorVars <- mapM (sequence . (, newName "successor")) successors

  ctx_   <- newName "ctx"
  block_ <- newName "block"
  attrs_ <- newName "attrs"
  
  sequenceA 
    [ sigD (mkName $ unpack $ opOpName operation) builderType
    , funD (mkName $ unpack $ opOpName operation) [ clause (varP ctx_ : varP block_ : [ varP n | (_, n) <- operandVars ] ++ [ varP n | (_, _, n) <- attributeVars ] ++ [ varP n | (_, n) <- regionVars] ++ [ varP n | (_, n) <- successorVars ])
                                                           (normalB [| undefined |])
                                                           [ valD (varP attrs_) (normalB [| undefined |]) []
                                                           ]
                                                  ]
    ]
  where getDagValueName (DagValueName (ValDef v) n) = Just (v, n)
        getDagValueName _ = Nothing
        getDagValue (DagValueName (ValDef v) _) = Just v
        getDagValue (DagValue     (ValDef v)  ) = Just v
        getDagValue _ = Nothing

generateDialect' :: Tablegen -> DecsQ
generateDialect' tblgen = fmap mconcat $ mapM (generateOperation' tblgen) $ getAllInstanceOf @Op tblgen

generateDialect :: [FilePath] -> FilePath -> DecsQ
generateDialect includedirs source = do
  tblgen <- runIO $ llvmTablegen includedirs source
  generateDialect' tblgen

generateMlirDialect :: FilePath -> DecsQ
generateMlirDialect subdir = do
  includdir <- runIO llvmIncludePath
  generateDialect [includdir] $ includdir </> subdir

maybeFail :: MonadFail m => String -> Maybe a -> m a
maybeFail errorLog = maybe (fail errorLog) return
