{-# LANGUAGE PatternSynonyms #-}
module Language.Goose.Typecheck.Definition.Type where
import Data.List
data Type
  = TVar Int
  | Int | Float | Void | Bool | Char
  | TId String
  | TApp Type [Type]
  | TRec [(String, Type)]
  deriving (Eq, Ord)

pattern Mutable :: Type -> Type
pattern Mutable t = TApp (TId "Mutable") [t]
  
pattern (:->) :: [Type] -> Type -> Type
pattern (:->) a b = TApp (TApp (TId "->") a) [b]

pattern TList :: Type -> Type
pattern TList a = TApp (TId "List") [a]

pattern TTuple :: [Type] -> Type
pattern TTuple a = TApp (TId "Tuple") a

instance Show Type where
  show (TVar i) = "a" ++ show i
  show Int = "int"
  show Float = "float"
  show Void = "void"
  show Bool = "bool"
  show Char = "char"
  show (TList Char) = "str"
  show (TList t) = "[" ++ show t ++ "]"
  show (TId x) = x
  show (TTuple a) = "(" ++ intercalate ", " (map show a) ++ ")"
  show (Mutable t) = "mut " ++ show t
  show (t1 :-> t2) = "fun(" ++ intercalate ", " (map show t1) ++ "): " ++ show t2
  show (TApp t1 t2) = "(" ++ show t1 ++ " " ++ unwords (map show t2) ++ ")"
  show (TRec fields) = "struct { " ++ unwords (map (\(n, t) -> n ++ ": " ++ show t) fields) ++ " }"

data Scheme = Forall [Int] Type
  deriving (Eq, Ord)

instance Show Scheme where
  show (Forall [] t) = show t
  show (Forall vs t) = "forall " ++ unwords (map show vs) ++ ". " ++ show t