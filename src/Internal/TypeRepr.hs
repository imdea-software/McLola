{-# Language GADTs #-}

module Internal.TypeRepr where
  
{-

  Type Representation

-}

-- || Type representation data type

data TypeRepr where
  TInt     :: TypeRepr
  TDouble  :: TypeRepr
  TBool    :: TypeRepr
  TChar    :: TypeRepr
  TMaybe   :: TypeRepr -> TypeRepr
  TEither  :: TypeRepr -> TypeRepr -> TypeRepr
  TList    :: TypeRepr -> TypeRepr
  TProd    :: TypeRepr -> TypeRepr -> TypeRepr
  TNAProd  :: String -> [(String, TypeRepr)] -> TypeRepr
  deriving (Eq, Ord)
  
-- Show instance
instance Show TypeRepr where
  show TInt          = "int"
  show TDouble       = "double"
  show TBool         = "bool"
  show TChar         = "character"
  show (TMaybe t)    = "MaybeM" ++ show t ++ "M"
  show (TEither t u) = "ETH" ++ show t ++ "OR" ++ show u ++ "HTE"
  show (TList t)     = "Ls" ++ show t ++ "sL"
  show (TProd t u)   = "PR" ++ show t ++ "X" ++ show u ++ "RP"
  show (TNAProd t _) = t

-- Built-in types
builtin :: TypeRepr -> Bool
builtin TInt    = True
builtin TDouble = True
builtin TBool   = True
builtin TChar   = True
builtin _       = False
