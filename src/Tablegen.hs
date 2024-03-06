{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Tablegen where
import Control.Monad

import Data.Aeson as A
import Data.Aeson.KeyMap (toMap)
import Data.Aeson.Key (toString, toText)

import Data.Vector hiding (mapM)
import Data.Map as M hiding ((!))
import Data.Set as S
import Data.Text as T
import Data.Maybe as J (fromMaybe, mapMaybe) 

data TblVal
  = ValUndefined
  | ValNumber    !Integer
  | ValString    !Text -- Maybe use Text
  | ValArray     !(Vector TblVal)
  | ValDef       !Def
  | ValVar       !Text
  | ValVarbit    !Text !Integer
  | ValDag       !Dag
  | ValComplex
getValNumber :: TblVal -> Maybe Integer
getValNumber (ValNumber i) = Just i
getValNumber _ = Nothing

getValString :: TblVal -> Maybe Text
getValString (ValString str) = Just str
getValString _ = Nothing

getValArray :: TblVal -> Maybe (Vector TblVal)
getValArray (ValArray arr) = Just arr
getValArray _ = Nothing

getValDef :: TblVal -> Maybe Def
getValDef (ValDef def) = Just def
getValDef _ = Nothing

getValDag :: TblVal -> Maybe Dag
getValDag (ValDag dag) = Just dag
getValDag _ = Nothing


instance FromJSON TblVal where
  parseJSON Null            = return ValUndefined
  parseJSON (A.Array arr)   = ValArray <$> mapM parseJSON arr
  parseJSON (String str)    = return $ ValString str
  parseJSON num@(Number _)  = ValNumber <$> parseJSON num
  parseJSON (Bool _)        = fail "Value cannot take boolean value"
  parseJSON (Object obj)    = do 
    kind :: String <- obj .: "kind"
    case kind of
      "def"     -> ValDef     <$> parseJSON (Object obj)
      "var"     -> ValVar     <$> obj .: "var"
      "varbit"  -> ValVarbit  <$> obj .: "var"
                              <*> obj .: "index"
      "dag"     -> ValDag     <$> parseJSON (Object obj)
      "complex" -> return ValComplex
      _         -> fail "Unrecognized value kind"

newtype Def = Def Text
instance FromJSON Def where
  parseJSON = withObject "def" $ \ def ->
    Def <$> def .: "def"
data DagArg
  = DagValue     !TblVal
  | DagValueName !TblVal !Text
  | DagName              !Text
instance FromJSON DagArg where
  parseJSON = withArray "DagArg" $ \ arg -> do
    value :: Maybe TblVal <- parseJSON $ arg ! 0
    name  :: Maybe Text   <- parseJSON $ arg ! 1
    case (value, name) of
      (Nothing, Just n) -> return $ DagName n
      (Just v, Nothing) -> return $ DagValue v
      (Just v, Just n)  -> return $ DagValueName v n
      _ -> fail "DagArg lacked both value and name"
data Dag = Dag { dagOperator :: !Def, dagArgs :: !(Vector DagArg) }
instance FromJSON Dag where
  parseJSON = withObject "Dag" $ \ dag ->
    Dag <$> dag .: "operator"
        <*> dag .: "args"

data Record = Record
  { superclasses :: Set Text
  , anonymous    :: Bool
  , name         :: Text
  , arguments    :: Map Text TblVal
  }
instance FromJSON Record where
  parseJSON = withObject "Record" $ \ record -> 
    Record <$> record .: "!superclasses"
           <*> record .: "!anonymous"
           <*> record .: "!name"
           <*> mapM parseJSON (filterWithKey (\ k _ -> not $ "!" `T.isPrefixOf` k) $
                               mapKeys toText $
                               toMap record)

data Tablegen = Tablegen
  { instanceof :: Map Text (Set Text)
  , records    :: Map Text Record
  }

instance FromJSON Tablegen where
  parseJSON = withObject "Tablegen" $ \ t ->
    Tablegen <$> t .: "!instanceof"
             <*> mapM parseJSON (filterWithKey (\ k _ -> not $ "!" `T.isPrefixOf` k) $
                                 mapKeys (pack . toString) $
                                 toMap t)

class RecordClass c where
  className :: Text
  reifyRecord :: Record -> Maybe c

isInstanceof :: forall c. RecordClass c => Record -> Bool
isInstanceof r = name r == className @c || S.member (className @c) (superclasses r)

defInstanceof :: forall c. RecordClass c => Tablegen -> Def -> Bool
defInstanceof t (Def def) = def == className @c || S.member def (fromMaybe S.empty $ M.lookup (className @c) $ instanceof t)

data Op = Op
  { opOpDialect  :: Def
  , opOpName     :: Text
  , opArguments  :: Dag
  , opResults    :: Dag
  , opRegions    :: Dag
  , opSuccessors :: Dag
  , opTraits     :: Vector Def
  }
instance RecordClass Op where
  className = "Op"
  reifyRecord record
    | isInstanceof @Op record =
      do
        let args = arguments record
        ValDef    opDialect  <- M.lookup "opDialect"  args
        ValString opName     <- M.lookup "opName"     args
        ValDag    arguments  <- M.lookup "arguments"  args
        ValDag    results    <- M.lookup "results"    args
        ValDag    regions    <- M.lookup "regions"    args
        ValDag    successors <- M.lookup "successors" args
        ValArray  traits'    <- M.lookup "traits"     args
        Op opDialect opName arguments results regions successors <$> mapM getValDef traits'
    | otherwise = Nothing

lookupDef :: Tablegen -> Def -> Maybe Record
lookupDef tablgen (Def def) = M.lookup def (records tablgen)

getAllInstanceOf :: forall a. RecordClass a => Tablegen -> [a]
getAllInstanceOf tblgen = J.mapMaybe (reifyRecord <=< flip M.lookup (records tblgen)) $ S.toList $ fromMaybe S.empty $ M.lookup (className @a) $ instanceof tblgen

data TypeConstraint 
instance RecordClass TypeConstraint where
  className = "TypeConstraint"
  reifyRecord r
    | isInstanceof @TypeConstraint r = Just undefined
    | otherwise = Nothing

data AttrConstraint
instance RecordClass AttrConstraint where
  className = "AttrConstraint"
  reifyRecord r
    | isInstanceof @AttrConstraint r = Just undefined
    | otherwise = Nothing

data VariadicRegion
instance RecordClass VariadicRegion where
  className = "VariadicRegion"
  reifyRecord r
    | isInstanceof @VariadicRegion r = Just undefined
    | otherwise = Nothing

data VariadicSuccessor
instance RecordClass VariadicSuccessor where
  className = "VariadicSuccessor"
  reifyRecord r
    | isInstanceof @VariadicSuccessor r = Just undefined
    | otherwise = Nothing

data Variadic
instance RecordClass Variadic where
  className = "Variadic"
  reifyRecord r
    | isInstanceof @Variadic r = Just undefined
    | otherwise = Nothing

newtype VariadicOfVariadic = VariadicOfVariadic
  { variadicOfVariadicSegmentAttrName :: Text
  }
instance RecordClass VariadicOfVariadic where
  className = "VariadicOfVariadic"
  reifyRecord r
    | isInstanceof @VariadicOfVariadic r =
      do
        ValString attrName <- M.lookup "segmentAttrName" $ arguments r
        return $ VariadicOfVariadic attrName
    | otherwise = Nothing

data Optional
instance RecordClass Optional where
  className = "Optional"
  reifyRecord r
    | isInstanceof @Optional r = Just undefined
    | otherwise = Nothing


data SameVariadicResultSize
instance RecordClass SameVariadicResultSize where
  className = "SameVariadicResultSize"
  reifyRecord r
    | isInstanceof @SameVariadicResultSize r = Just undefined
    | otherwise = Nothing

data SameVariadicOperandSize
instance RecordClass SameVariadicOperandSize where
  className = "SameVariadicOperandSize"
  reifyRecord r
    | isInstanceof @SameVariadicOperandSize r = Just undefined
    | otherwise = Nothing

data AttrSizedOperandSegments
instance RecordClass AttrSizedOperandSegments where
  className = "AttrSizedOperandSegments"
  reifyRecord r
    | isInstanceof @AttrSizedOperandSegments r = Just undefined
    | otherwise = Nothing

data OptionalAttr
instance RecordClass OptionalAttr where
  className = "OptionalAttr"
  reifyRecord r
    | isInstanceof @OptionalAttr r = Just undefined
    | otherwise = Nothing

newtype Dialect = Dialect Text
instance RecordClass Dialect where
  className = "Dialect"
  reifyRecord r
    | isInstanceof @Dialect r =
      do
      let args = arguments r
      ValString dname <- M.lookup "name" args
      return (Dialect dname)
    | otherwise = Nothing
