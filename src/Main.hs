module Main where

import Compile

import System.IO
import System.Environment

{-

  Instead of using the interpreter, you can compile the project.

 Run:
 $ ghc Main.hs

 Generate monitor files:
 $ ./Main --analyse
 $ ./Main --codegen

 To delete files generated by compilation, use the clean script:
 $ ./clean

-}

data RunMode = Analysis | CodeGen | ShowHelp

main :: IO ()
main = parseArgs <$> getArgs >>= runInMode

parseArgs :: [String] -> RunMode
parseArgs ["--analyse"] = Analysis
parseArgs ["--codegen"] = CodeGen
parseArgs _             = ShowHelp

runInMode :: RunMode -> IO ()
runInMode Analysis = analyse
runInMode CodeGen  = codegen
runInMode ShowHelp = putStr$unlines [
    "Wrong arguments. Usage:"
  , "  --analyse to generate static analysis information file"
  , "  --codegen to generate c code for spec"
  , "Modify Compile.hs to specify the imported specification to work with."]
