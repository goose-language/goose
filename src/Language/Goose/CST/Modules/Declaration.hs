module Language.Goose.CST.Modules.Declaration where
import Data.List

type Name = String
data Namespaced = Simple Name | Namespaced [Name] Name
  deriving (Eq, Ord)

instance Show Namespaced where
  show (Simple x) = x
  show (Namespaced xs x) = intercalate "::" (xs ++ [x])

data Declaration
  -- | A type identifier, used to represent a type constructor
  -- | Of the form: x
  = ID Namespaced

  -- | A type constructor, used to represent a type constructor
  -- | Of the form: x y
  | Constructor Declaration [Declaration]

  -- | A type variable, used to represent a type constructor
  -- | Of the form: α
  | Generic String

  -- | A list type, used to represent a type constructor
  -- | Of the form: [α]
  | List Declaration
  
  -- | Some basic primitive types
  | Int | Float | Bool | Char | Unit
  deriving Eq

instance Show Declaration where
  show (ID x) = show x
  show (List x) = "[" ++ show x ++ "]"
  show Int = "int"
  show Float = "float"
  show Bool = "bool"
  show Char = "char"
  show Unit = "()"
  show (Generic x) = x
  show (Constructor x xs) = show x ++ "[" ++ intercalate ", " (map show xs) ++ "]"