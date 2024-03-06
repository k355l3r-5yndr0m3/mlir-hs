{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module MLIR.TH
( llvmIncludePath
, llvmTablegen
, generateDialect
, generateMlirDialect
) where
import Prelude hiding (map)
import MLIR.FFI.BuiltinAttributes
import MLIR.FFI.IR
import MLIR.IR

import Tablegen

import Control.Monad (when)
import Control.Exception

import Data.Aeson
import Data.Coerce
import Data.Int       (Int32)
import Data.String    (IsString(fromString))
import Data.Either    (partitionEithers)
import Data.Text      (unpack, Text, map)
import Data.Maybe     (fromMaybe, catMaybes)
import Data.Bifunctor (first, second)
import Data.Primitive.PrimArray (primArrayFromListN, primArrayFromList)
import Data.List      (genericLength)

import System.Process (shell, readCreateProcess)
import System.FilePath

import Language.Haskell.TH

import qualified Data.Vector as V

llvmIncludePath :: IO FilePath
llvmIncludePath = init <$> readCreateProcess (shell "llvm-config --includedir ") ""

llvmTablegen :: [FilePath] -> FilePath -> IO Tablegen
llvmTablegen includedirs source = do
  jsonString <- readCreateProcess (shell $ "llvm-tblgen --dump-json " ++ unwords ["-I"++dir | dir <- includedirs] ++ " " ++ source) ""
  case eitherDecode (fromString jsonString) of
    Left errmsg -> fail errmsg
    Right tblgen -> return tblgen

data OperandInfo = OpVariadic    Bool (Maybe Text)
                 | OpNonVariadic Bool
                 deriving (Eq)

getOperandInfo :: Tablegen -> Def -> OperandInfo
getOperandInfo tblgen def
  | defInstanceof @Variadic tblgen def
    = OpVariadic    isOptional $ coerce $ reifyRecord @VariadicOfVariadic =<< lookupDef tblgen def
  | otherwise
    = OpNonVariadic isOptional
  where isOptional = defInstanceof @Optional tblgen def

data OperationTraits = OperationTraits { sameVariadicOperandSize :: Bool, attrSizedOperandSegments :: Bool, sameVariadicResultSize :: Bool }
getOperationTraits :: Tablegen -> Op -> OperationTraits
getOperationTraits tblgen op =
  foldl (\ t d -> 
          let t' | defInstanceof @SameVariadicResultSize tblgen d = t { sameVariadicResultSize = True }
                 | defInstanceof @SameVariadicOperandSize tblgen d = t { sameVariadicOperandSize = True }
                 | defInstanceof @AttrSizedOperandSegments tblgen d = t { attrSizedOperandSegments = True }
                 | otherwise = t
          in  t')
        (OperationTraits False False False)
        traits
  where traits = opTraits op

generateOperation' :: Tablegen -> Op -> DecsQ
generateOperation' tblgen operation = do
  Dialect dn <- maybeFail (unpack (opOpName operation) ++ " dialect"   ) $ reifyRecord =<< lookupDef tblgen (opOpDialect operation)
  args       <- maybeFail (unpack (opOpName operation) ++ " args"      ) $ fmap V.toList $ mapM getDagValueMaybeName $ dagArgs $ opArguments  operation
  results    <- maybeFail (unpack (opOpName operation) ++ " results"   ) $ fmap (fmap (defInstanceof @Variadic tblgen) . V.toList) $ mapM getDagValue $ dagArgs $ opResults    operation
  regions    <- maybeFail (unpack (opOpName operation) ++ " regions"   ) $ fmap (fmap (defInstanceof @VariadicRegion tblgen) . V.toList) $ mapM getDagValue $ dagArgs $ opRegions    operation
  successors <- maybeFail (unpack (opOpName operation) ++ " successors") $ fmap (fmap (defInstanceof @VariadicSuccessor tblgen) . V.toList) $ mapM getDagValue $ dagArgs $ opSuccessors operation
  -- The arguments to the builder is in (context -> block -> location -> [operands] -> [attributes] -> [Regions] -> [Successor] -> [Return types] -> )

  (unzip -> (operands, operandNames), attributes) 
    <- partitionEithers <$> maybeFail (unpack (opOpName operation) ++ " operands and attributes") 
                                      (mapM (\case (v, Just n)  | defInstanceof @TypeConstraint tblgen v
                                                                            -> Just $ Left (getOperandInfo tblgen v, n)
                                                                | otherwise -> Just $ Right (defInstanceof @OptionalAttr tblgen v, n)
                                                   (v, Nothing) | defInstanceof @TypeConstraint tblgen v 
                                                                            -> Just $ Left (getOperandInfo tblgen v, "_anonymous_")
                                                                | otherwise -> Nothing
                                            ) args)
  -- let (unzip -> (operands, operandNames), attributes) = partitionEithers $ fmap (\ (v, n) -> 
  --                                                         if defInstanceof @TypeConstraint tblgen v then
  --                                                           Left (getOperandInfo tblgen v, n)
  --                                                         else
  --                                                           Right (defInstanceof @OptionalAttr tblgen v, n))
  --                                                      args
  let builderType = 
        let resultType | null results = [t| IO MlirOperation |]
                       | otherwise    = [t| IO $(foldl (\ t r ->
                                                          if r then
                                                            [t| $t [MlirValue] |]
                                                          else
                                                            [t| $t  MlirValue  |]) [t| $(tupleT $ length results + 1) MlirOperation |] results) |]
            rtypes     = foldr (\ r t -> if r then 
                                           [t| [MlirType] -> $t |] 
                                         else 
                                           [t|  MlirType  -> $t |]) resultType results
            succs      = foldr (\ r t -> 
                                  if r then
                                    [t| [MlirBlock] -> $t |]
                                  else
                                    [t|  MlirBlock  -> $t |]) rtypes successors
            regs       = foldr (\ r t -> 
                                  if r then
                                    [t| [MlirRegion -> IO ()] -> $t |]
                                  else
                                    [t| (MlirRegion -> IO ()) -> $t |]) succs regions
            attrs      = foldr (\ (o, _) t -> [t| $(if o then 
                                                      [t| Maybe MlirAttribute |]
                                                    else [t| MlirAttribute |]
                                                   ) -> $t |]) regs attributes
            opers      = foldr (\ v t -> [t| $(case v of OpNonVariadic optional
                                                          -> (if optional then appT [t| Maybe |] else id) [t| MlirValue |]
                                                         OpVariadic optional Nothing 
                                                          -> (if optional then appT [t| Maybe |] else id) [t| [MlirValue] |]
                                                         OpVariadic optional (Just _)
                                                          -> (if optional then appT [t| Maybe |] else id) [t| [[MlirValue]] |]
                                              ) -> $t |]) attrs operands
        in  [t| MlirContext -> MlirBlock -> MlirLocation -> $opers |]
  operandVars   <- mapM (sequence . (, newName "operand")) operands
  attributeVars <- mapM (\ (v, n) -> (v, n,) <$> newName ("attribute_" ++ unpack n)) attributes
  regionVars    <- mapM (sequence . (, newName "region")) regions
  successorVars <- mapM (sequence . (, newName "successor")) successors
  rtypeVars     <- mapM (sequence . (, newName "rtype")) results

  ctx_     <- newName "ctx"
  block_   <- newName "block"
  loc_     <- newName "loc"
  attrs_   <- newName "attrs"
  opers_   <- newName "operands"
  regs_    <- newName "regions"
  succs_   <- newName "succs"
  rtyps_   <- newName "rettypes"
  rget_    <- newName "resultget"
  segms_   <- newName "segment"
  vsattrs_ <- newName "varSegmAttr"
  sleng_   <- newName "sameLength"

  let (numVariadicResult, numSimpleResult) = foldr (\v -> if v then first (+1) else second (+1)) (0, 0) results
      rgetf | null results = [| fst |]
            | numVariadicResult > 0 =
              do
                when (numVariadicResult > 1 && not (sameVariadicResultSize traits)) $
                  fail $ "Invalid Op(" <> unpack (opOpName operation) <> "): Missing SameVariadicResultSize trait which is required when operation have multiple variadic results"
                op_ <- newName "op"
                rs_ <- newName "rs"
                sg_ <- newName "sg"
                t1_ <- newName "t1"
                tv_ <- newName "tv"
                let rgeter []     e _ = e
                    rgeter (a:as) e r | a         =
                                      do
                                        a_  <- newName "a"
                                        as_ <- newName "as"
                                        e'_ <- newName "e'"
                                        [| let ($(varP a_), $(varP as_)) = $(varE tv_) $r
                                               $(varP e'_) = $e $(varE a_)
                                           in  $(rgeter as (varE e'_) (varE as_)) |]
                                      | otherwise = 
                                      do 
                                        a_  <- newName "a"
                                        as_ <- newName "as"
                                        e'_ <- newName "e'"
                                        [| let ($(varP a_), $(varP as_)) = $(varE t1_) $r
                                               $(varP e'_) = $e $(varE a_)
                                           in  $(rgeter as (varE e'_) (varE as_)) |]
                lam1E (tupP [varP op_, varP rs_])
                     [| let $(varP sg_) = (length $(varE rs_) - $(litE $ integerL numSimpleResult)) `div` $(litE $ integerL numVariadicResult)
                            $(varP t1_) = \ r@(a:as) -> assert (not $ null r) (a, as)
                            $(varP tv_) = \ r        -> assert (length r >= $(varE sg_)) (splitAt $(varE sg_) r)
                            rmk = $(do 
                                      names <- mapM (const $ newName "r") results
                                      lamE (fmap varP names) (tupE $ fmap varE $ op_:names))
                        in  $(rgeter results [| rmk |] (varE rs_)) |]
            | otherwise =
              do
                op_ <- newName "op"
                rs_ <- newName "rs"
                lam1E (tupP [varP op_, varP rs_]) $ tupE $ varE op_ : fmap (\ idx -> [| $(varE rs_) !! $(litE $ integerL idx) |]) [0..numSimpleResult-1]

  sequenceA 
    [ sigD (mkName normalizedOpName) builderType
    , funD_doc
        (mkName normalizedOpName) 
        [ clause (varP ctx_ : varP block_ : varP loc_ :
                 [ varP n | (_, n)    <- operandVars   ]
              ++ [ varP n | (_, _, n) <- attributeVars ]
              ++ [ varP n | (_, n)    <- regionVars    ]
              ++ [ varP n | (_, n)    <- successorVars ]
              ++ [ varP n | (_, n)    <- rtypeVars     ])
             (normalB
                  [| $(varE rget_) <$> mlirBlockAddOperation $(varE block_)
                                                             $(litE $ stringL $ unpack $ dn <> "." <> opOpName operation)
                                                             $(varE loc_)
                                                             $(varE rtyps_)
                                                             $(if sameVariadicOperandSize traits then [| (assert $(varE sleng_) $(varE opers_)) |] else [| $(varE opers_) |])
                                                             $(varE regs_)
                                                             $(varE succs_)
                                                             $(varE attrs_)
                                                             False
                  |]
             )
             [ valD (varP attrs_)
                    (normalB $ foldr (\ (optional, n, u) e ->
                                        if optional then
                                          [| maybe id ((:) . mlirNamedAttributeGet (mlirIdentifierGet $(varE ctx_) $(litE $ stringL $ unpack n))) $(varE u) $e |]
                                        else 
                                          [| mlirNamedAttributeGet (mlirIdentifierGet $(varE ctx_) $(litE $ stringL $ unpack n)) $(varE u) : $e |]
                                     )
                                     (if attrSizedOperandSegments traits then [| $(varE segms_) : $(varE vsattrs_) |] else [| $(varE vsattrs_) |]) -- TODO: Move this somewhere else
                                     attributeVars)
                    []
             , valD (varP opers_)
                    (normalB $ foldr (\ (v, n) e -> 
                                        case v of
                                          OpNonVariadic optional
                                            | optional  -> [| maybe id (:) $(varE n) $e |]
                                            | otherwise -> [| $(varE n) : $e |]
                                          OpVariadic    optional Nothing  ->
                                            [| $(if optional then [| fromMaybe [] |] else [| id |]) $(varE n) ++ $e |]
                                          OpVariadic    optional (Just _) ->
                                            [| $(if optional then [| maybe [] concat |] else [| concat |]) $(varE n) ++ $e |]
                                          ) [| [] |] operandVars)
                    []
             , valD (varP regs_)
                    (normalB $ foldr (\ (v, n) e -> 
                                        if v then 
                                          [| $(varE n) ++ $e |]
                                        else 
                                          [| $(varE n) : $e |]) [| [] |] regionVars)
                    []
             , valD (varP succs_)
                    (normalB $ foldr (\ (v, n) e ->
                                        if v then 
                                          [| $(varE n) ++ $e |]
                                        else 
                                          [| $(varE n) : $e |])
                                      [| [] |]
                                      successorVars)
                    []
             , valD (varP rtyps_)
                    (normalB $ foldr (\ (v, n) e -> 
                                        if v then
                                          [| $(varE n) ++ $e |]
                                        else
                                          [| $(varE n) :  $e |])
                                     [| [] |]
                                     rtypeVars)
                    []
             , valD (varP rget_)
                    (normalB rgetf)
                    []
             , valD (varP segms_) -- If this is not used then the compiler will probably optimize it away
                    (normalB (if attrSizedOperandSegments traits then
                                [| mlirNamedAttributeGet (mlirIdentifierGet $(varE ctx_) "operandSegmentSizes") $
                                   mlirDenseI32ArrayGet $(varE ctx_) $
                                                        primArrayFromListN $(litE $ integerL $ fromIntegral $ length operandVars)
                                                                           $(foldr (\ (oinfo, name) e ->
                                                                                      case oinfo of
                                                                                        OpVariadic optional Nothing
                                                                                          | optional  -> [| maybe 0 genericLength $(varE name) : $e |]
                                                                                          | otherwise -> [| genericLength $(varE name) : $e |]
                                                                                        OpVariadic optional (Just _)
                                                                                          | optional  -> [| maybe 0 (genericLength . concat) $(varE name) : $e |]
                                                                                          | otherwise -> [| genericLength (concat $(varE name)) : $e |]
                                                                                        OpNonVariadic optional 
                                                                                          | optional  -> [| maybe 0 (const 1) $(varE name) : $e |]
                                                                                          | otherwise -> [| 1 : $e |]
                                                                                    )
                                                                                    [| [] :: [Int32] |]
                                                                                    operandVars
                                                                            )
                                |]
                              else
                                [| error "Code generation implementation error. This should not have been evaluated." |]
                             )
                    )
                    []
             , valD (varP vsattrs_)
                    (normalB $ foldr (\ (oinfo, name) e -> 
                                        case oinfo of
                                          OpVariadic optional (Just attrName)
                                            -> [| mlirNamedAttributeGet (mlirIdentifierGet $(varE ctx_) $(litE $ stringL $ unpack attrName))
                                                                        (mlirDenseI32ArrayGet $(varE ctx_) $ 
                                                                            primArrayFromList 
                                                                            $(if optional then
                                                                                [| maybe [] (fmap genericLength) $(varE name) |]
                                                                              else
                                                                                [| fmap genericLength $(varE name) |] 
                                                                             )
                                                                        )
                                                  : $e
                                               |]
                                          _otherwise -> e
                                     )
                                     [| [] |]
                                     operandVars)
                    []
             , do 
               let variadicOperandLengthExps = catMaybes [ case opinfo of
                                                             OpVariadic optional Nothing
                                                               | optional  -> Just [| maybe 0 length $(varE name) |]
                                                               | otherwise -> Just [| length $(varE name) |]
                                                             OpVariadic optional (Just _)
                                                               | optional  -> Just [| maybe 0 (sum . fmap length) $(varE name) |]
                                                               | otherwise -> Just [| sum $ fmap length $(varE name) |]
                                                             OpNonVariadic optional
                                                               | optional  -> Just [| maybe 0 (const 1) $(varE name) |]
                                                               | otherwise -> Nothing
                                                         | (opinfo, name) <- operandVars ]
               oln <- mapM (sequence . (, newName "voln")) variadicOperandLengthExps
               valD (varP sleng_)
                    (normalB $ if sameVariadicOperandSize traits then
                                 do
                                 metric <- case oln of []        -> fail "Op has SameVariadicOperandSize but does not have a variadic operand."
                                                       ((_,a):_) -> return a
                                 foldr (\ (_, name) e -> [| ($(varE name) == $(varE metric)) && $e |])
                                       [| True |]
                                       oln
                               else
                                 [| error "Code generation implementation error. This should not have been evaluated." |])
                    [valD (varP name) (normalB expr) [] | (expr, name) <- oln]
             ]
        ]
        (Just $ unwords $ [unpack $ opOpName operation, "context", "block", "location"] ++ [unpack name | name <- operandNames]
                                                                                        ++ [unpack name | (_, name) <- attributes]
                                                                                        ++ ["region" | _ <- regions]
                                                                                        ++ ["successor" | _ <- successors]
                                                                                        ++ ["result-type" | _ <- results]) $
        [Just "Owning context", Just "Owning block", Just "Location"] ++ [Just $ unpack name | name <- operandNames]
                                                                      ++ [Just $ unpack name | (_, name) <- attributes]
                                                                      ++ [Just "" | _ <- regions]
                                                                      ++ [Just "" | _ <- successors]
                                                                      ++ [Just "" | _ <- results]
    ]
  where 
        getDagValue (DagValueName (ValDef v) _) = Just v
        getDagValue (DagValue     (ValDef v)  ) = Just v
        getDagValue _ = Nothing
        getDagValueMaybeName (DagValueName (ValDef v) n) = Just (v, Just n)
        getDagValueMaybeName (DagValue     (ValDef v)  ) = Just (v, Nothing)
        getDagValueMaybeName _ = Nothing
        traits = getOperationTraits tblgen operation
        normalizedOpName = ('_':) $ unpack $ map (\case '.' -> '_'; ch -> ch) $ opOpName operation -- TODO: Find a better naming scheme


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
