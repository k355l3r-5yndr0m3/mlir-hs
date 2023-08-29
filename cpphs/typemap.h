#define _MaybeUnitAttr Maybe UnitAttr
#define _MaybeIntegerAttr Maybe IntegerAttr
#define _MaybeBoolAttr Maybe BoolAttr
#define _MaybeStringAttr Maybe StringAttr
#define _MaybeArrayAttr Maybe ArrayAttr

#define _TypeAttr forall t. TypeGet t => TypeAttr t
#define _TypedAttr forall t. TypedAttr t => t
#define _ElementsAttr forall t. ElementsAttr t => t

#define _DenseIntElementsAttr forall t. DenseIntElementsAttr t => t

#define _MaybeDenseIntElementsAttr forall t. DenseIntElementsAttr t => Maybe t
#define _MaybeDenseElementsAttr forall t. DenseElementsAttr t => Maybe t
#define _FastMathFlagsAttr FastMathFlagsAttr
#define _CmpIPredicateAttr CmpIPredicateAttr
#define _IntegerAttr IntegerAttr
#define _CmpFPredicateAttr CmpFPredicateAttr
#define _StringAttr StringAttr
#define _FlatSymbolRefAttr FlatSymbolRefAttr

#define _DenseI32ArrayAttr DenseI32ArrayAttr

#define _BoolAttr BoolAttr
#define _FloatAttr forall t. TypeGet t => FloatAttr t 
