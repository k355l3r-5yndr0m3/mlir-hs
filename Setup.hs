import Distribution.Simple
import Distribution.Simple.Setup

import System.Directory
import System.FilePath

import MLIR.TblGen


baseHooks :: UserHooks
baseHooks = autoconfUserHooks

userHooks :: UserHooks
userHooks = baseHooks { 
  preBuild = (\args buildFlags -> do
    let dialectOutput  = fromFlag (buildDistPref buildFlags) </> "build" </> "autogen" </> "MLIR" </> "Dialect"
        dialectInclude = fromFlag (buildDistPref buildFlags) </> "build" </> "include"
    createDirectoryIfMissing True dialectOutput
    -- Func dialect
    createDirectoryIfMissing True $ dialectOutput </> "Func"
    hsGenerate [dialectInclude] 
               (dialectInclude </> "mlir" </> "Dialect" </> "Func" </> "IR"</> "FuncOps" <.> "td")
               "MLIR.Dialect.Func.Ops"
               []
               (dialectOutput </> "Func" </> "Ops" <.> "hs")
    -- Arith dialect
    createDirectoryIfMissing True $ dialectOutput </> "Arith"
    hsGenerate [dialectInclude] 
               (dialectInclude </> "mlir" </> "Dialect" </> "Arith" </> "IR" </> "ArithOps" <.> "td")
               "MLIR.Dialect.Arith.Ops"
               ["MLIR.Dialect.Arith.Attributes"]
               (dialectOutput </> "Arith" </> "Ops" <.> "hs")

    -- Math dialect 
    createDirectoryIfMissing True $ dialectOutput </> "Math"
    hsGenerate [dialectInclude] 
               (dialectInclude </> "mlir" </> "Dialect" </> "Math" </> "IR" </> "MathOps" <.> "td")
               "MLIR.Dialect.Math.Ops"
               ["MLIR.Dialect.Arith.Attributes"]
               (dialectOutput </> "Math" </> "Ops" <.> "hs")
    -- Async dialect
    createDirectoryIfMissing True $ dialectOutput </> "Async"
    hsGenerate [dialectInclude] 
               (dialectInclude </> "mlir" </> "Dialect" </> "Async" </> "IR" </> "AsyncOps" <.> "td")
               "MLIR.Dialect.Async.Ops"
               []
               (dialectOutput </> "Async" </> "Ops" <.> "hs")
    -- ControlFlow dialect
    createDirectoryIfMissing True $ dialectOutput </> "ControlFlow"
    hsGenerate [dialectInclude] 
               (dialectInclude </> "mlir" </> "Dialect" </> "ControlFlow" </> "IR" </> "ControlFlowOps" <.> "td")
               "MLIR.Dialect.ControlFlow.Ops"
               []
               (dialectOutput </> "ControlFlow" </> "Ops" <.> "hs")
    -- GPU dialect
    createDirectoryIfMissing True $ dialectOutput </> "GPU"
    hsGenerate [dialectInclude] 
               (dialectInclude </> "mlir" </> "Dialect" </> "GPU" </> "IR" </> "GPUOps" <.> "td")
               "MLIR.Dialect.GPU.Ops"
               ["MLIR.Dialect.GPU.Attributes"]
               (dialectOutput </> "GPU" </> "Ops" <.> "hs")
    -- Linalg dialect
    createDirectoryIfMissing True $ dialectOutput </> "Linalg"
    hsGenerate [dialectInclude] 
               (dialectInclude </> "mlir" </> "Dialect" </> "Linalg" </> "IR" </> "LinalgOps" <.> "td")
               "MLIR.Dialect.Linalg.Ops"
               []
               (dialectOutput </> "Linalg" </> "Ops" <.> "hs")
    hsGenerate [dialectInclude] 
               (dialectInclude </> "mlir" </> "Dialect" </> "Linalg" </> "IR" </> "LinalgStructuredOps" <.> "td")
               "MLIR.Dialect.Linalg.StructuredOps"
               ["MLIR.Dialect.Linalg.Attributes"]
               (dialectOutput </> "Linalg" </> "StructuredOps" <.> "hs")

    -- LLVM
    -- createDirectoryIfMissing True $ dialectOutput </> "LLVM"
    -- hsGenerate [dialectInclude] 
    --            dialectInclude </> "mlir" </> "Dialect" </> "LLVMIR" </> "LLVMOps.td"
    --            "MLIR.Dialect.LLVM.Ops"
    --            ["MLIR.Dialect.LLVM.Attributes"]
    --            (dialectOutput </> "LLVM" </> "Ops" <.> "hs")
    -- MemRef 
    createDirectoryIfMissing True $ dialectOutput </> "MemRef"
    hsGenerate [dialectInclude] 
               (dialectInclude </> "mlir" </> "Dialect" </> "MemRef" </> "IR" </> "MemRefOps" <.> "td")
               "MLIR.Dialect.MemRef.Ops"
               ["MLIR.Dialect.MemRef.Attributes"]
               (dialectOutput </> "MemRef" </> "Ops" <.> "hs")

    -- MLProgram 
    createDirectoryIfMissing True $ dialectOutput </> "MLProgram"
    hsGenerate [dialectInclude] 
               (dialectInclude </> "mlir" </> "Dialect" </> "MLProgram" </> "IR" </> "MLProgramOps" <.> "td")
               "MLIR.Dialect.MLProgram.Ops"
               []
               (dialectOutput </> "MLProgram" </> "Ops" <.> "hs")
    -- PDL
    createDirectoryIfMissing True $ dialectOutput </> "PDL"
    hsGenerate [dialectInclude] 
               (dialectInclude </> "mlir" </> "Dialect" </> "PDL" </> "IR" </> "PDLOps" <.> "td")
               "MLIR.Dialect.PDL.Ops"
               []
               (dialectOutput </> "PDL" </> "Ops" <.> "hs")


    -- Quant
    createDirectoryIfMissing True $ dialectOutput </> "Quant"
    hsGenerate [dialectInclude] 
               (dialectInclude </> "mlir" </> "Dialect" </> "Quant" </> "QuantOps" <.> "td")
               "MLIR.Dialect.Quant.Ops"
               []
               (dialectOutput </> "Quant" </> "Ops" <.> "hs")

    -- SCF
    createDirectoryIfMissing True $ dialectOutput </> "SCF"
    hsGenerate [dialectInclude] 
               (dialectInclude </> "mlir" </> "Dialect" </> "SCF" </> "IR" </> "SCFOps" <.> "td")
               "MLIR.Dialect.SCF.Ops"
               []
               (dialectOutput  </> "SCF" </> "Ops" <.> "hs")

    -- Shape
    createDirectoryIfMissing True $ dialectOutput </> "Shape"
    hsGenerate [dialectInclude] 
               (dialectInclude </> "mlir" </> "Dialect" </> "Shape" </> "IR" </> "ShapeOps" <.> "td")
               "MLIR.Dialect.Shape.Ops"
               []
               (dialectOutput </> "Shape" </> "Ops" <.> "hs")
    
      
    -- SparseTensor
    -- createDirectoryIfMissing True $ dialectOutput </> "SparseTensor"
    -- hsGenerate [dialectInclude] 
    --            dialectInclude </> "mlir" </> "Dialect" </> "SparseTensor" </> "IR" </> "SparseTensorOps" <.> "td"
    --            "MLIR.Dialect.SparseTensor.Ops"
    --            []
    --            (dialectOutput </> "SparseTensor" </> "Ops" <.> "hs")

    -- Tensor
    createDirectoryIfMissing True $ dialectOutput </> "Tensor"
    hsGenerate [dialectInclude] 
               (dialectInclude </> "mlir" </> "Dialect" </> "Tensor" </> "IR" </> "TensorOps" <.> "td")
               "MLIR.Dialect.Tensor.Ops"
               []
               (dialectOutput </> "Tensor" </> "Ops" <.> "hs")

    -- Transform
    -- createDirectoryIfMissing True $ dialectOutput </> "Transform"
    -- hsGenerate [dialectInclude] 
    --            dialectInclude </> "mlir" </> "Dialect" </> "Transform" </> "IR" </> "TransformOps.td"
    --            "MLIR.Dialect.Transform.Ops"
    --            []
    --            (dialectOutput </> "Transform" </> "Ops" <.> "hs")

    -- Vector
    -- createDirectoryIfMissing True $ dialectOutput </> "Vector"
    -- hsGenerate [dialectInclude] 
    --            dialectInclude </> "mlir" </> "Dialect" </> "Vector" </> "IR" </> "VectorOps.td"
    --            "MLIR.Dialect.Vector.Ops"
    --            []
    --            (dialectOutput </> "Vector" </> "Ops" <.> "hs")


    -- Do the other things
    preBuild baseHooks args buildFlags) }


{-
 -
    createDirectoryIfMissing True $ dialectOutput </> "DL"
    hsGenerate [dialectInclude] 
               "dialectInclude/mlir/Dialect/DL/IR/DLOps.td"
               "MLIR.Dialect.DL.Ops"
               []
               (dialectOutput </> "DL" </> "Ops" <.> "hs")
 -
 - -}






main :: IO ()
main = defaultMainWithHooks userHooks
