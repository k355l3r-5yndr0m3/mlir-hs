module MLIR.Dialect.GPU.Attributes where
import MLIR.IR

type DimensionAttr = Attribute
type AllReduceOperationAttr = Attribute
type TransposeModeAttr = Attribute
type ShuffleModeAttr = Attribute
type MMAElementwiseOpAttr = Attribute
type Prune2To4SpMatFlagAttr = Attribute
type SpGEMMWorkEstimationOrComputeKindAttr = Attribute
