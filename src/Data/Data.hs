module Data.Data (compileData) where

import Data.Buffer          (decBuffer, fundefBuffer)
import Data.TypeGen         (decType, fundefType)
import Internal.Maps        (TypeMap, getType)
import Internal.TypeRepr    (TypeRepr)
import Internal.C99         (toC99Type, getIncludes)

import Language.C99.Simple
import Language.C99.Pretty  (pretty)
import Text.PrettyPrint     (render)
import Data.Map             (foldr)

{-

  Code generation for C99 data types
  
  The compileData function generates two files that implement the C99
  data types required for the specification:
  
  data.h: - declaration of types and their functions prototypes
          - declaration of input struct to handle input values
          - declaration of buffer types and their function prototypes
  data.c: - implementation of types and buffer functions

-}

-- || Types declaration

fieldsDec :: TypeMap -> [Ident] -> [FieldDecln]
fieldsDec tyMap = map fieldDec
  where
    fieldDec s = FieldDecln (toC99Type (getType s tyMap)) s

ctypesh :: [Ident] -> TypeMap -> [TypeRepr] -> [TypeRepr] -> TransUnit
ctypesh input tyMap types buffTypes = TransUnit (typDec ++ inpDec ++ buffDec) []
  where
    typDec  = Prelude.foldr (++) [] (map decType types)
    inpDec  = [inputDec, inputTypeDef]
    buffDec = Prelude.foldr (++) [] (map decBuffer buffTypes)
    inputDec     = TypeDecln (TypeSpec (StructDecln (Just "input") (fieldsDec tyMap input)))
    inputTypeDef = VarDecln (Just Typedef) (TypeSpec (Struct "input")) "input" Nothing


-- || Types implementation

ctypesc :: [TypeRepr] -> [TypeRepr] -> TransUnit
ctypesc types buffTypes = TransUnit [] (typFun ++ buffFun)
  where
    typFun  = Prelude.foldr (++) [] (map fundefType types)
    buffFun = Prelude.foldr (++) [] (map fundefBuffer buffTypes)
 

-- || Code generation

-- Compile to data.h and data.c files

compile :: String -> [String] -> TransUnit -> TransUnit -> IO ()
compile filename includes tuh tuc = do
  let hfile      = render $ pretty $ translate $ tuh
      cfile      = render $ pretty $ translate $ tuc
      hguards    = unlines [ "#ifndef " ++ filename ++ "_H"
                           , "#define " ++ filename ++ "_H"
                           , ""
                           , "#include <stdio.h>"]
      hguardsEnd = unlines ["", "#endif"]
      hmacros    = unlines ((map (\inc -> "#include " ++ inc) includes) ++ [""])
      cmacros    = unlines [ "#include \"" ++ filename ++ ".h\"", ""]
  writeFile ("monitor/" ++ filename ++ ".h") $ hguards ++ hmacros ++ hfile ++ hguardsEnd
  writeFile ("monitor/" ++ filename ++ ".c") $ cmacros ++ cfile

compileData :: [Ident] -> TypeMap -> [TypeRepr] -> IO ()
compileData input typeMap types =
       compile "data" includes tuh tuc
  where
    incl      = Prelude.foldr (++) [] (map getIncludes types)
    includes  = if (elem "<stdbool.h>" incl) then incl else "<stdbool.h>":incl
    tuh       = ctypesh input typeMap types buffTypes
    tuc       = ctypesc types buffTypes
    buffTypes = Data.Map.foldr (\ty tys -> if (elem ty tys) then tys else ty:tys) [] typeMap
