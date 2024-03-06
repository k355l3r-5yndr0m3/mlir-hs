{-# LANGUAGE TemplateHaskell #-}
module MLIR.Dialect.PDL
( module MLIR.Dialect.PDL
, module Extra
)where
import MLIR.FFI.IR
import MLIR.TH
import MLIR.Dialect.PDL.Extra as Extra

foreign import ccall unsafe "mlirGetDialectHandle__pdl__"
  dialectHandle :: MlirDialectHandle
$(generateMlirDialect "mlir/Dialect/PDL/IR/PDLOps.td")
