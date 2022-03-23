module Compile (analyse, codegen) where

import Examples.UAV.UAV1              (spec)
import StaticAnalysis.StaticAnalysis  (analysis, outputAnalysis)
import Data.Data                      (compileData)
import Engine.Engine                  (compileEngine)

{-
 
  Use the GHCi interpreter to generate C files that implement
  a monitor for the importedSpec specification:

 $ ghci
 > :l Compile.hs
 ...
 > analyse
 > codegen

-}

-- || Specification

importedSpec     = spec
externalIncludes = ["lib.h", "math.h"]

-- || Static Analysis

(dgraph, latMap, size, ordSpec, typeMap, types, input) = analysis importedSpec

analyse :: IO ()
analyse = outputAnalysis dgraph latMap

-- || Code Generation

codegen :: IO ()
codegen =    compileData input typeMap types
          >> compileEngine ordSpec size typeMap latMap externalIncludes
