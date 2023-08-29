{-# OPTIONS_GHC -pgmPgcc -optP-E -optP-DHASKELL #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE ForeignFunctionInterface #-}
module MLIR.Dialect.Arith.Attributes (
  FastMathFlagsAttr
, FastMathFlags(..)
, CmpIPredicateAttr(..)
, CmpFPredicateAttr(..)
) where
import Prelude hiding (EQ)

import MLIR.C.IR
import MLIR.IR

import Data.Bits
import Foreign

import GHC.IsList

#include <Dialect/arith.cpp>
data FastMathFlags = NONE | REASSOC | NNAN | NINF | NSZ | ARCP | CONTRACT | AFN
fastMathFlagsBit :: FastMathFlags -> Word32
fastMathFlagsBit NONE     = hs__mlirArithFastMathFlags__none
fastMathFlagsBit REASSOC  = hs__mlirArithFastMathFlags__reassoc
fastMathFlagsBit NNAN     = hs__mlirArithFastMathFlags__nnan
fastMathFlagsBit NINF     = hs__mlirArithFastMathFlags__ninf
fastMathFlagsBit NSZ      = hs__mlirArithFastMathFlags__nsz
fastMathFlagsBit ARCP     = hs__mlirArithFastMathFlags__arcp
fastMathFlagsBit CONTRACT = hs__mlirArithFastMathFlags__contract
fastMathFlagsBit AFN      = hs__mlirArithFastMathFlags__afn

instance IsList FastMathFlagsAttr where
  type Item FastMathFlagsAttr = FastMathFlags

  fromList (fmap fastMathFlagsBit -> flags) = FastMathFlagsAttr $ foldr (.|.) hs__mlirArithFastMathFlags__none flags
  toList (FastMathFlagsAttr flags) = filter (\ (fastMathFlagsBit -> b) -> (flags .&. b) /= 0 ) [NONE, REASSOC, NNAN, NINF, NSZ, ARCP, CONTRACT, AFN]

newtype FastMathFlagsAttr = FastMathFlagsAttr Word32
instance AttrGet FastMathFlagsAttr where
  attrGet (FastMathFlagsAttr flags) c = fastMathFlagsAttrGet c flags

data CmpIPredicateAttr = IPredEQ | IPredNE | IPredSLT | IPredSLE | IPredSGT | IPredSGE | IPredULT | IPredULE | IPredUGT | IPredUGE
instance AttrGet CmpIPredicateAttr where
  attrGet IPredEQ  = hs__mlirArithCmpIPredicate__eq
  attrGet IPredNE  = hs__mlirArithCmpIPredicate__ne
  attrGet IPredSLT = hs__mlirArithCmpIPredicate__slt
  attrGet IPredSLE = hs__mlirArithCmpIPredicate__sle
  attrGet IPredSGT = hs__mlirArithCmpIPredicate__sgt
  attrGet IPredSGE = hs__mlirArithCmpIPredicate__sge
  attrGet IPredULT = hs__mlirArithCmpIPredicate__ult
  attrGet IPredULE = hs__mlirArithCmpIPredicate__ule
  attrGet IPredUGT = hs__mlirArithCmpIPredicate__ugt
  attrGet IPredUGE = hs__mlirArithCmpIPredicate__uge

data CmpFPredicateAttr = FPredAlwaysFalse | FPredOEQ | FPredOGT | FPredOGE | FPredOLT | FPredOLE | FPredONE 
                   | FPredORD | FPredUEQ | FPredUGT | FPredUGE | FPredULT | FPredULE | FPredUNE | FPredUNO | FPredAlwaysTrue
instance AttrGet CmpFPredicateAttr where
  attrGet FPredAlwaysFalse = hs__mlirArithCmpFPredicate__AlwaysFalse
  attrGet FPredOEQ         = hs__mlirArithCmpFPredicate__OEQ       
  attrGet FPredOGT         = hs__mlirArithCmpFPredicate__OGT       
  attrGet FPredOGE         = hs__mlirArithCmpFPredicate__OGE       
  attrGet FPredOLT         = hs__mlirArithCmpFPredicate__OLT       
  attrGet FPredOLE         = hs__mlirArithCmpFPredicate__OLE       
  attrGet FPredONE         = hs__mlirArithCmpFPredicate__ONE       
  attrGet FPredORD         = hs__mlirArithCmpFPredicate__ORD       
  attrGet FPredUEQ         = hs__mlirArithCmpFPredicate__UEQ       
  attrGet FPredUGT         = hs__mlirArithCmpFPredicate__UGT       
  attrGet FPredUGE         = hs__mlirArithCmpFPredicate__UGE       
  attrGet FPredULT         = hs__mlirArithCmpFPredicate__ULT       
  attrGet FPredULE         = hs__mlirArithCmpFPredicate__ULE       
  attrGet FPredUNE         = hs__mlirArithCmpFPredicate__UNE       
  attrGet FPredUNO         = hs__mlirArithCmpFPredicate__UNO       
  attrGet FPredAlwaysTrue  = hs__mlirArithCmpFPredicate__AlwaysTrue
