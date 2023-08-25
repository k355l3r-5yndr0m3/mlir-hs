{-# LANGUAGE PatternSynonyms #-}
-- TODO: Remove the *Get function, make c function for each enum
module MLIR.Dialect.Arith.Attributes (
  fastMathFlagsNone
, fastMathFlagsReassoc
, fastMathFlagsNnan 
, fastMathFlagsNinf
, fastMathFlagsNsz
, fastMathFlagsArcp
, fastMathFlagsContract
, fastMathFlagsAfn
, fastMathFlagsAttrGet

, cmpIPredicateEq 
, cmpIPredicateNe 
, cmpIPredicateSlt
, cmpIPredicateSle
, cmpIPredicateSgt
, cmpIPredicateSge
, cmpIPredicateUlt
, cmpIPredicateUle
, cmpIPredicateUgt
, cmpIPredicateUge
, cmpIPredicateAttrGet 

, cmpFPredicateAlwaysFalse
, cmpFPredicateOEQ
, cmpFPredicateOGT
, cmpFPredicateOGE
, cmpFPredicateOLT
, cmpFPredicateOLE
, cmpFPredicateONE
, cmpFPredicateORD
, cmpFPredicateUEQ
, cmpFPredicateUGT
, cmpFPredicateUGE
, cmpFPredicateULT
, cmpFPredicateULE
, cmpFPredicateUNE
, cmpFPredicateUNO
, cmpFPredicateAlwaysTrue
, cmpFPredicateAttrGet

, FastMathFlagsAttr
, CmpIPredicateAttr
, CmpFPredicateAttr
) where
import qualified MLIR.C.IR as C
import MLIR.IR

import Foreign 

-- TODO: Change this
type FastMathFlagsAttr = Attribute
foreign import ccall unsafe "hs__mlirArithFastMathFlagsAttrGet"
  mlirArithFastMathFlagsAttrGet' :: C.Context -> Word32 -> IO C.Attribute

foreign import ccall unsafe "hs__mlirArithFastMathFlags__none" 
  mlirArithFastMathFlags__none :: Word32
foreign import ccall unsafe "hs__mlirArithFastMathFlags__reassoc" 
  mlirArithFastMathFlags__reassoc :: Word32
foreign import ccall unsafe "hs__mlirArithFastMathFlags__nnan" 
  mlirArithFastMathFlags__nnan :: Word32
foreign import ccall unsafe "hs__mlirArithFastMathFlags__ninf" 
  mlirArithFastMathFlags__ninf :: Word32
foreign import ccall unsafe "hs__mlirArithFastMathFlags__nsz" 
  mlirArithFastMathFlags__nsz :: Word32
foreign import ccall unsafe "hs__mlirArithFastMathFlags__arcp" 
  mlirArithFastMathFlags__arcp :: Word32
foreign import ccall unsafe "hs__mlirArithFastMathFlags__contract" 
  mlirArithFastMathFlags__contract :: Word32
foreign import ccall unsafe "hs__mlirArithFastMathFlags__afn" 
  mlirArithFastMathFlags__afn :: Word32
newtype FastMathFlags = FastMathFlags Word32 
                      deriving(Eq, Bits)

fastMathFlagsNone :: FastMathFlags
fastMathFlagsNone = FastMathFlags mlirArithFastMathFlags__none
fastMathFlagsReassoc :: FastMathFlags
fastMathFlagsReassoc = FastMathFlags mlirArithFastMathFlags__reassoc
fastMathFlagsNnan :: FastMathFlags
fastMathFlagsNnan = FastMathFlags mlirArithFastMathFlags__nnan
fastMathFlagsNinf :: FastMathFlags
fastMathFlagsNinf = FastMathFlags mlirArithFastMathFlags__ninf
fastMathFlagsNsz :: FastMathFlags
fastMathFlagsNsz = FastMathFlags mlirArithFastMathFlags__nsz
fastMathFlagsArcp :: FastMathFlags
fastMathFlagsArcp = FastMathFlags mlirArithFastMathFlags__arcp
fastMathFlagsContract :: FastMathFlags
fastMathFlagsContract = FastMathFlags mlirArithFastMathFlags__contract
fastMathFlagsAfn :: FastMathFlags 
fastMathFlagsAfn = FastMathFlags mlirArithFastMathFlags__afn

fastMathFlagsAttrGet :: FastMathFlags -> FastMathFlagsAttr
fastMathFlagsAttrGet (FastMathFlags flags) = Attribute $ \ctx ->
  mlirArithFastMathFlagsAttrGet' ctx flags


type CmpIPredicateAttr = Attribute
foreign import ccall unsafe "hs__mlirArithCmpIPredicateAttrGet"
  mlirArithCmpIPredicateAttrGet' :: C.Context -> Word64 -> IO C.Attribute
cmpIPredicateAttrGet :: CmpIPredicate -> CmpIPredicateAttr
cmpIPredicateAttrGet (CmpIPredicate val) = Attribute $ \ctx ->
  mlirArithCmpIPredicateAttrGet' ctx val
  
foreign import ccall unsafe "hs__mlirArithCmpIPredicate__eq"
  mlirArithCmpIPredicate__eq :: Word64
foreign import ccall unsafe "hs__mlirArithCmpIPredicate__ne"
  mlirArithCmpIPredicate__ne :: Word64
foreign import ccall unsafe "hs__mlirArithCmpIPredicate__slt"
  mlirArithCmpIPredicate__slt :: Word64
foreign import ccall unsafe "hs__mlirArithCmpIPredicate__sle"
  mlirArithCmpIPredicate__sle :: Word64
foreign import ccall unsafe "hs__mlirArithCmpIPredicate__sgt"
  mlirArithCmpIPredicate__sgt :: Word64
foreign import ccall unsafe "hs__mlirArithCmpIPredicate__sge"
  mlirArithCmpIPredicate__sge :: Word64
foreign import ccall unsafe "hs__mlirArithCmpIPredicate__ult"
  mlirArithCmpIPredicate__ult :: Word64
foreign import ccall unsafe "hs__mlirArithCmpIPredicate__ule"
  mlirArithCmpIPredicate__ule :: Word64
foreign import ccall unsafe "hs__mlirArithCmpIPredicate__ugt"
  mlirArithCmpIPredicate__ugt :: Word64
foreign import ccall unsafe "hs__mlirArithCmpIPredicate__uge"
  mlirArithCmpIPredicate__uge :: Word64
newtype CmpIPredicate = CmpIPredicate Word64

cmpIPredicateEq :: CmpIPredicate
cmpIPredicateEq = CmpIPredicate mlirArithCmpIPredicate__eq
cmpIPredicateNe :: CmpIPredicate
cmpIPredicateNe = CmpIPredicate mlirArithCmpIPredicate__ne
cmpIPredicateSlt :: CmpIPredicate
cmpIPredicateSlt = CmpIPredicate mlirArithCmpIPredicate__slt
cmpIPredicateSle :: CmpIPredicate
cmpIPredicateSle = CmpIPredicate mlirArithCmpIPredicate__sle
cmpIPredicateSgt :: CmpIPredicate
cmpIPredicateSgt = CmpIPredicate mlirArithCmpIPredicate__sgt
cmpIPredicateSge :: CmpIPredicate
cmpIPredicateSge = CmpIPredicate mlirArithCmpIPredicate__sge
cmpIPredicateUlt :: CmpIPredicate
cmpIPredicateUlt = CmpIPredicate mlirArithCmpIPredicate__ult
cmpIPredicateUle :: CmpIPredicate
cmpIPredicateUle = CmpIPredicate mlirArithCmpIPredicate__ule
cmpIPredicateUgt :: CmpIPredicate
cmpIPredicateUgt = CmpIPredicate mlirArithCmpIPredicate__ugt
cmpIPredicateUge :: CmpIPredicate
cmpIPredicateUge = CmpIPredicate mlirArithCmpIPredicate__uge

type CmpFPredicateAttr = Attribute
newtype CmpFPredicate = CmpFPredicate Word64
foreign import ccall unsafe "hs__mlirArithCmpFPredicateAttrGet"
  mlirArithCmpFPredicateAttrGet' :: C.Context -> Word64 -> IO C.Attribute
cmpFPredicateAttrGet :: CmpFPredicate -> CmpFPredicateAttr
cmpFPredicateAttrGet (CmpFPredicate val) = Attribute $ \ctx ->
  mlirArithCmpFPredicateAttrGet' ctx val

foreign import ccall "hs__mlirArithCmpFPredicate__AlwaysFalse" mlirArithCmpFPredicate__AlwaysFalse :: Word64
cmpFPredicateAlwaysFalse :: CmpFPredicate
cmpFPredicateAlwaysFalse = CmpFPredicate mlirArithCmpFPredicate__AlwaysFalse
foreign import ccall "hs__mlirArithCmpFPredicate__OEQ" mlirArithCmpFPredicate__OEQ :: Word64
cmpFPredicateOEQ :: CmpFPredicate
cmpFPredicateOEQ = CmpFPredicate mlirArithCmpFPredicate__OEQ
foreign import ccall "hs__mlirArithCmpFPredicate__OGT" mlirArithCmpFPredicate__OGT :: Word64
cmpFPredicateOGT :: CmpFPredicate
cmpFPredicateOGT = CmpFPredicate mlirArithCmpFPredicate__OGT
foreign import ccall "hs__mlirArithCmpFPredicate__OGE" mlirArithCmpFPredicate__OGE :: Word64
cmpFPredicateOGE :: CmpFPredicate
cmpFPredicateOGE = CmpFPredicate mlirArithCmpFPredicate__OGE
foreign import ccall "hs__mlirArithCmpFPredicate__OLT" mlirArithCmpFPredicate__OLT :: Word64
cmpFPredicateOLT :: CmpFPredicate
cmpFPredicateOLT = CmpFPredicate mlirArithCmpFPredicate__OLT
foreign import ccall "hs__mlirArithCmpFPredicate__OLE" mlirArithCmpFPredicate__OLE :: Word64
cmpFPredicateOLE :: CmpFPredicate
cmpFPredicateOLE = CmpFPredicate mlirArithCmpFPredicate__OLE
foreign import ccall "hs__mlirArithCmpFPredicate__ONE" mlirArithCmpFPredicate__ONE :: Word64
cmpFPredicateONE :: CmpFPredicate
cmpFPredicateONE = CmpFPredicate mlirArithCmpFPredicate__ONE
foreign import ccall "hs__mlirArithCmpFPredicate__ORD" mlirArithCmpFPredicate__ORD :: Word64
cmpFPredicateORD :: CmpFPredicate
cmpFPredicateORD = CmpFPredicate mlirArithCmpFPredicate__ORD
foreign import ccall "hs__mlirArithCmpFPredicate__UEQ" mlirArithCmpFPredicate__UEQ :: Word64
cmpFPredicateUEQ :: CmpFPredicate
cmpFPredicateUEQ = CmpFPredicate mlirArithCmpFPredicate__UEQ
foreign import ccall "hs__mlirArithCmpFPredicate__UGT" mlirArithCmpFPredicate__UGT :: Word64
cmpFPredicateUGT :: CmpFPredicate
cmpFPredicateUGT = CmpFPredicate mlirArithCmpFPredicate__UGT
foreign import ccall "hs__mlirArithCmpFPredicate__UGE" mlirArithCmpFPredicate__UGE :: Word64
cmpFPredicateUGE :: CmpFPredicate
cmpFPredicateUGE = CmpFPredicate mlirArithCmpFPredicate__UGE
foreign import ccall "hs__mlirArithCmpFPredicate__ULT" mlirArithCmpFPredicate__ULT :: Word64
cmpFPredicateULT :: CmpFPredicate
cmpFPredicateULT = CmpFPredicate mlirArithCmpFPredicate__ULT
foreign import ccall "hs__mlirArithCmpFPredicate__ULE" mlirArithCmpFPredicate__ULE :: Word64
cmpFPredicateULE :: CmpFPredicate
cmpFPredicateULE = CmpFPredicate mlirArithCmpFPredicate__ULE
foreign import ccall "hs__mlirArithCmpFPredicate__UNE" mlirArithCmpFPredicate__UNE :: Word64
cmpFPredicateUNE :: CmpFPredicate
cmpFPredicateUNE = CmpFPredicate mlirArithCmpFPredicate__UNE
foreign import ccall "hs__mlirArithCmpFPredicate__UNO" mlirArithCmpFPredicate__UNO :: Word64
cmpFPredicateUNO :: CmpFPredicate
cmpFPredicateUNO = CmpFPredicate mlirArithCmpFPredicate__UNO
foreign import ccall "hs__mlirArithCmpFPredicate__AlwaysTrue" mlirArithCmpFPredicate__AlwaysTrue :: Word64
cmpFPredicateAlwaysTrue :: CmpFPredicate
cmpFPredicateAlwaysTrue = CmpFPredicate mlirArithCmpFPredicate__AlwaysTrue
