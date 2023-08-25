#include <cstdint>
#include <mlir-c/IR.h>
#include <mlir/CAPI/IR.h>
#include <mlir/Dialect/Arith/IR/Arith.h>

#define HS_ENUM_VALUE(type, hname, cname, value) extern "C" type hs__ ## hname ## __ ## value() { return (type) cname::value; }


static_assert(sizeof(mlir::arith::FastMathFlags) == sizeof(uint32_t), "mlir::arith::FastMathFlags bitwidth changed.");
#define HS_ENUM_ARITH_FASTMATHFLAGS(value) HS_ENUM_VALUE(uint32_t, mlirArithFastMathFlags, mlir::arith::FastMathFlags, value)
HS_ENUM_ARITH_FASTMATHFLAGS(none)
HS_ENUM_ARITH_FASTMATHFLAGS(reassoc)
HS_ENUM_ARITH_FASTMATHFLAGS(nnan)
HS_ENUM_ARITH_FASTMATHFLAGS(ninf)
HS_ENUM_ARITH_FASTMATHFLAGS(nsz)
HS_ENUM_ARITH_FASTMATHFLAGS(arcp)
HS_ENUM_ARITH_FASTMATHFLAGS(contract)
HS_ENUM_ARITH_FASTMATHFLAGS(afn)

extern "C" MlirAttribute hs__mlirArithFastMathFlagsAttrGet(MlirContext context, uint32_t flags) {
    return wrap(mlir::arith::FastMathFlagsAttr::get(unwrap(context), (mlir::arith::FastMathFlags)flags));
}

// TODO: Make better
static_assert(sizeof(mlir::arith::CmpIPredicate) == sizeof(uint64_t), "mlir::arith::CmpIPredicate bitwidth changed.");
#define HS_ENUM_ARITH_CMPIPREDICATE(value) HS_ENUM_VALUE(uint64_t, mlirArithCmpIPredicate, mlir::arith::CmpIPredicate, value)
HS_ENUM_ARITH_CMPIPREDICATE(eq)
HS_ENUM_ARITH_CMPIPREDICATE(ne)
HS_ENUM_ARITH_CMPIPREDICATE(slt)
HS_ENUM_ARITH_CMPIPREDICATE(sle)
HS_ENUM_ARITH_CMPIPREDICATE(sgt)
HS_ENUM_ARITH_CMPIPREDICATE(sge)
HS_ENUM_ARITH_CMPIPREDICATE(ult)
HS_ENUM_ARITH_CMPIPREDICATE(ule)
HS_ENUM_ARITH_CMPIPREDICATE(ugt)
HS_ENUM_ARITH_CMPIPREDICATE(uge)

extern "C" MlirAttribute hs__mlirArithCmpIPredicateAttrGet(MlirContext context, uint64_t val) {
    return wrap(mlir::arith::CmpIPredicateAttr::get(unwrap(context), (mlir::arith::CmpIPredicate)val));
}


static_assert(sizeof(mlir::arith::CmpFPredicate) == sizeof(uint64_t), "mlir::arith::CmpFPredicate bitwidth changed.");
#define HS_ENUM_ARITH_CMPFPREDICATE(value) HS_ENUM_VALUE(uint64_t, mlirArithCmpFPredicate, mlir::arith::CmpFPredicate, value)
HS_ENUM_ARITH_CMPFPREDICATE(AlwaysFalse);
HS_ENUM_ARITH_CMPFPREDICATE(OEQ);
HS_ENUM_ARITH_CMPFPREDICATE(OGT);
HS_ENUM_ARITH_CMPFPREDICATE(OGE);
HS_ENUM_ARITH_CMPFPREDICATE(OLT);
HS_ENUM_ARITH_CMPFPREDICATE(OLE);
HS_ENUM_ARITH_CMPFPREDICATE(ONE);
HS_ENUM_ARITH_CMPFPREDICATE(ORD);
HS_ENUM_ARITH_CMPFPREDICATE(UEQ);
HS_ENUM_ARITH_CMPFPREDICATE(UGT);
HS_ENUM_ARITH_CMPFPREDICATE(UGE);
HS_ENUM_ARITH_CMPFPREDICATE(ULT);
HS_ENUM_ARITH_CMPFPREDICATE(ULE);
HS_ENUM_ARITH_CMPFPREDICATE(UNE);
HS_ENUM_ARITH_CMPFPREDICATE(UNO);
HS_ENUM_ARITH_CMPFPREDICATE(AlwaysTrue);
extern "C" MlirAttribute hs__mlirArithCmpFPredicateAttrGet(MlirContext context, uint64_t val) {
    return wrap(mlir::arith::CmpFPredicateAttr::get(unwrap(context), (mlir::arith::CmpFPredicate)val));
}
