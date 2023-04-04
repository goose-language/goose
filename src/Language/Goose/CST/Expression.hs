{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Goose.CST.Expression where
import Language.Goose.CST.Located ( Located )
import Language.Goose.CST.Annoted ( Annoted(Annoted) )
import Language.Goose.CST.Literal ( Literal )
import Language.Goose.CST.Modules.Pattern ( Pattern )
import Language.Goose.CST.Modules.Declaration ( Declaration, Name, Namespaced )
import Data.List ( intercalate )

data Toplevel
  -- | A toplevel function declaration 
  -- | Of the form: def functionName(args) body end
  = Function { 
      functionName :: Name,
      functionArgs :: [String],
      functionBody :: Located Expression }

  -- | A toplevel public declaration
  -- | Of the form: pub declaration
  | Public (Located Toplevel)

  -- | A toplevel enumeration declaration
  -- | Of the form: enum enumerationName n₁(x₁), n₂(x₂), ..., nₙ(xₙ) end
  | Enumeration {
      enumerationName :: Name,
      enumerationGens :: [String],
      enumerationMembers :: [Annoted [Declaration]] }

  -- | A namespace declaration
  -- | Of the form: namespace namespaceName { ... }
  | Namespace {
      namespaceName :: Name,
      namespaceBody :: [Located Toplevel] }
  
  -- | An import declaration
  -- | Of the form: import moduleName
  | Import Name

  -- | An import-as declaration
  -- | Of the form: import moduleName as alias
  | ImportAs Name Name

  -- | An extern declaration
  -- | Of the form: extern declaration
  | Extern {
      externName :: Name,
      externGenerics :: [String],
      externArgs :: [Declaration],
      externReturn :: Declaration }

  -- | A function declaration
  -- | Of the form: declare functionName(args): ret
  | Declare {
      declareName :: Name,
      declareGenerics :: [String],
      declareArgs :: [Declaration],
      declareReturn :: Declaration }
  deriving Eq
  
data Expression
  = Variable Namespaced
  | Literal Literal

  -- | An expression application, used to represent a function applied to an argument
  -- | Of the form: f(x₀, x₁, ..., xₙ)
  | Application (Located Expression) [Located Expression]

  -- | A lambda abstraction, used to represent a function that takes arguments
  -- | Of the form: fun(x₀, x₁, ..., xₙ) e
  | Lambda { 
      lambdaArgs :: [String],
      lambdaBody :: Located Expression }

  -- | A let expression, used to represent a local binding
  -- | Of the form: let x: t = e₁ in e₂
  | Let String (Located Expression) (Located Expression)
  
  -- | A pattern match expression, used to represent a case expression
  -- | Of the form: match e { p₀ = e₀, p₁ = e₁, ..., pₙ = eₙ }
  | Match (Located Expression) [(Located Pattern, Located Expression)]

  -- | A statement sequence
  -- | Of the form: { e₀; e₁; ...; eₙ }
  | Sequence [Located Expression]

  -- | A dereference expression, used to represent a value at an address in memory
  -- | Of the form: *variable
  | Dereference (Located Expression)

  -- | A list expression
  -- | Of the form: [e₀, e₁, ..., eₙ]
  | List [Located Expression]

  -- | A structure expression
  -- | Of the form: { x₀ = e₀, x₁ = e₁, ..., xₙ = eₙ }
  | Structure [(String, Located Expression)]

  -- | A structure access expression, used to represent a field of a structure
  -- | Of the form: e.x

  | StructureAccess (Located Expression) String

  -- | An index expression, used to represent an index into a list
  -- | Of the form: e₀[e₁]
  | ListAccess (Located Expression) (Located Expression)

  -- | A Binary expression
  -- | Of the form: e₁ op e₂
  | Binary String (Located Expression) (Located Expression)

  -- | A while loop expression
  -- | Of the form: while e₁ e₂
  | While (Located Expression) [Located Expression]

  -- | A for loop expression
  -- | Of the form: for x: t = e₁ to e₂ e₃
  | For {
      forVariable :: String,
      forIn :: Located Expression,
      forBody :: [Located Expression] }
  
  -- | A conditional expression
  -- | Of the form: if e₁ then e₂ else e₃
  -- | Or of the form: e₁ ? e₂ : e₃
  | If (Located Expression) (Located Expression) (Maybe (Located Expression))

  -- | A variable update expression
  -- | Of the form: x = e
  | Update (Located Updated) (Located Expression)
  
  | Return (Located Expression)
  deriving Eq

data Updated
  = VariableUpdate Namespaced
  | ListUpdate (Located Updated) (Located Expression)
  | StructureUpdate (Located Updated) String
  deriving Eq

instance Show Toplevel where
  show (Function name args body) = "def " ++ name ++ "(" ++ intercalate ", " args ++ ")" ++ show body ++ " end"
  show (Public toplevel) = "public " ++ show toplevel
  show (Enumeration name gens members) = "enum " ++ name ++ show gens ++ " { " ++ intercalate ", " (map (\(Annoted name' tys) -> if null tys then name' else name' ++ "(" ++ intercalate ", " (map show tys) ++ ")") members) ++ " }"
  show (Namespace name body) = "module " ++ name ++ " " ++ intercalate "; " (map show body) ++ " end"
  show (Extern name gens decls ret) = "extern " ++ name ++ show gens ++ "(" ++ intercalate ", " (map show decls) ++ "): " ++ show ret
  show (Import name) = "import " ++ name
  show (ImportAs name alias) = "import " ++ name ++ " as " ++ alias
  show (Declare name gens args ret) = "def " ++ name ++ show gens ++ "(" ++ intercalate ", " (map show args) ++ "): " ++ show ret

instance Show Expression where
  show (Variable name) = show name
  show (Literal literal) = show literal
  show (Application function args) = show function ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show (Let variable body expression) = "def " ++ show variable ++ " = " ++ show body ++ " in " ++ show expression
  show (Match expression cases) = "match " ++ show expression ++ " { " ++ intercalate ", " (map (\(pattern, body) -> show pattern ++ " = " ++ show body) cases) ++ " }"
  show (Sequence expressions) = "do " ++ intercalate "; " (map show expressions) ++ " end"
  show (Dereference expression) = "*" ++ show expression
  show (List expressions) = "[" ++ intercalate ", " (map show expressions) ++ "]"
  show (ListAccess list index) = show list ++ "[" ++ show index ++ "]"
  show (While condition body) = "while " ++ show condition ++ " " ++ show body
  show (For variable inExpr body) = "for " ++ variable ++ " in " ++ show inExpr ++ " " ++ show body
  show (If condition then_ else_) = "if " ++ show condition ++ " then " ++ show then_ ++ " else " ++ show else_
  show (Update variable expression) = show variable ++ " = " ++ show expression
  show (Binary op left right) = show left ++ " " ++ op ++ " " ++ show right
  show (Return expression) = "return " ++ show expression
  show (Structure fields) = "{" ++ intercalate ", " (map (\(name, expression) -> name ++ " = " ++ show expression) fields) ++ "}"
  show (StructureAccess structure field) = show structure ++ "." ++ field
  show (Lambda args body) = "fun(" ++ intercalate ", " args ++ ") " ++ show body

instance Show Updated where
  show (VariableUpdate name) = show name
  show (ListUpdate variable index) = show variable ++ "[" ++ show index ++ "]"
  show (StructureUpdate variable field) = show variable ++ "." ++ field