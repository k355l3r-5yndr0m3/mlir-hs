#ifndef HASKELL
#include <cstdint>
#include <mlir-c/IR.h>
#include <mlir/CAPI/IR.h>
#include <mlir/Dialect/Arith/IR/Arith.h>
#endif

#ifndef HASKELL
#define ENUM_ATTR_GETTER(hname, cname, value) extern "C" MlirAttribute hs__##hname##__##value(MlirContext context) { return wrap(cname##Attr::get(unwrap(context), cname::value)); }
#define FLAG_ATTR_GETTER(width, hname, cname, value) extern "C" uint##width##_t hs__##hname##__##value(void) { return (uint##width##_t) cname::value; }


static_assert(sizeof(mlir::arith::FastMathFlags) == sizeof(uint32_t), "Width mismatch!");
#else 
#define STRINGIFY(str) #str
#define ENUM_ATTR_GETTER(hname, cname, value) foreign import ccall unsafe STRINGIFY(hs__##hname##__##value) hs__##hname##__##value :: Context -> IO Attribute
#define FLAG_ATTR_GETTER(width, hname, cname, value) foreign import ccall STRINGIFY(hs__##hname##__##value) hs__##hname##__##value :: Word##width
#endif

#define FASTMATHFLAGS(value) FLAG_ATTR_GETTER(32, mlirArithFastMathFlags, mlir::arith::FastMathFlags, value)
FASTMATHFLAGS(none);
FASTMATHFLAGS(reassoc);
FASTMATHFLAGS(nnan);
FASTMATHFLAGS(ninf);
FASTMATHFLAGS(nsz);
FASTMATHFLAGS(arcp);
FASTMATHFLAGS(contract);
FASTMATHFLAGS(afn);
#ifndef HASKELL
extern "C" MlirAttribute hs__mlirArithFastMathFlagsAttrGet(MlirContext context, uint32_t flags) { return wrap(mlir::arith::FastMathFlagsAttr::get(unwrap(context), (mlir::arith::FastMathFlags)flags)); }
#else
foreign import ccall unsafe "hs__mlirArithFastMathFlagsAttrGet" fastMathFlagsAttrGet :: Context -> Word32 -> IO Attribute
#endif


#define CMPIPREDICATE(value) ENUM_ATTR_GETTER(mlirArithCmpIPredicate, mlir::arith::CmpIPredicate, value)
CMPIPREDICATE(eq);
CMPIPREDICATE(ne);
CMPIPREDICATE(slt);
CMPIPREDICATE(sle);
CMPIPREDICATE(sgt);
CMPIPREDICATE(sge);
CMPIPREDICATE(ult);
CMPIPREDICATE(ule);
CMPIPREDICATE(ugt);
CMPIPREDICATE(uge);

#define CMPFPREDICATE(value) ENUM_ATTR_GETTER(mlirArithCmpFPredicate, mlir::arith::CmpFPredicate, value)
CMPFPREDICATE(AlwaysFalse);
CMPFPREDICATE(OEQ);
CMPFPREDICATE(OGT);
CMPFPREDICATE(OGE);
CMPFPREDICATE(OLT);
CMPFPREDICATE(OLE);
CMPFPREDICATE(ONE);
CMPFPREDICATE(ORD);
CMPFPREDICATE(UEQ);
CMPFPREDICATE(UGT);
CMPFPREDICATE(UGE);
CMPFPREDICATE(ULT);
CMPFPREDICATE(ULE);
CMPFPREDICATE(UNE);
CMPFPREDICATE(UNO);
CMPFPREDICATE(AlwaysTrue);

