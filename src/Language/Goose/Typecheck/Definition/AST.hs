{-# LANGUAGE PatternSynonyms #-}
module Language.Goose.Typecheck.Definition.AST where
import Language.Goose.CST.Literal
import Language.Goose.CST.Annoted
import Language.Goose.Typecheck.Definition.Type hiding (pattern Mutable)
import Data.List

type Name = String

data Toplevel
  -- | A toplevel function declaration 
  -- | Of the form: fun functionName[generics](args): returnType = body
  = Function {
      functionName :: Name,
      functionArgs :: [Annoted Type],
      functionReturn :: Type,
      functionBody :: Expression }

  -- | A toplevel declaration
  -- | Of the form: let x: t = e
  | Declaration {
      declarationName :: Name,
      declarationType :: Type,
      declarationBody :: Expression }

  | Declare [Type] (Annoted Type)
  deriving Eq

data Expression
  = Variable Name Type
  | Literal Literal

  -- | An expression application, used to represent a function applied to an argument
  -- | Of the form: f(x₀, x₁, ..., xₙ)
  | Application Expression [Expression]

  -- | A lambda abstraction, used to represent a function that takes arguments
  -- | Of the form: fun(x₀: t₀, x₁: t₁, ..., xₙ: tₙ) e
  | Lambda {
      lambdaArgs :: [Annoted Type],
      lambdaBody :: Expression }

  -- | A let expression, used to represent a local binding
  -- | Of the form: let x: t = e₁ in e₂
  | Let (Annoted Type) Expression Expression

  -- | A pattern match expression, used to represent a case expression
  -- | Of the form: match e { p₀ = e₀, p₁ = e₁, ..., pₙ = eₙ }
  | Match Expression [(Pattern, Expression)]

  -- | A statement sequence
  -- | Of the form: { e₀; e₁; ...; eₙ }
  | Sequence [Expression]

  -- | A list expression
  -- | Of the form: [e₀, e₁, ..., eₙ]
  | List [Expression]

  -- | An index expression, used to represent an index into a list
  -- | Of the form: e₀[e₁]
  | ListAccess Expression Expression

  -- | A while loop expression
  -- | Of the form: while e₁ e₂
  | While Expression [Expression]

  -- | A for loop expression
  -- | Of the form: for x: t = e₁ to e₂ e₃
  | For {
      forVariable :: Annoted Type,
      forIn :: Expression,
      forBody :: [Expression] }

  -- | A conditional expression
  -- | Of the form: if e₁ then e₂ else e₃
  -- | Or of the form: e₁ ? e₂ : e₃
  | If Expression Expression (Maybe Expression)

  -- | A variable update expression
  -- | Of the form: x = e
  | Update Updated Expression

  -- | A return expression
  -- | Of the form: return e
  | Return Expression

  -- | A binary expression
  -- | Of the form: e₁ op e₂
  | Binary String Expression Expression

  -- | A structure expression
  -- | Of the form: { x₀ = e₀, x₁ = e₁, ..., xₙ = eₙ }
  | Structure [(Name, Expression)]

  -- | A structure access expression
  -- | Of the form: e.x
  | StructureAccess Expression Name
  deriving Eq

data Updated
  = VariableUpdate Name Type
  | ListUpdate Updated Expression
  | StructureUpdate Updated Name
  deriving Eq

data Pattern
  = PVariable String Type
  | PLiteral Literal
  | PStructure [(Name, Pattern)]
  | PList [Pattern]
  | PConstructor String [Pattern]
  | PWildcard
  deriving Eq

instance Show Toplevel where
  show (Function name args returnType body) = "fun " ++ name ++ show args ++ show returnType ++ " = " ++ show body
  show (Declaration name type_ body) = "let " ++ name ++ ": " ++ show type_ ++ " = " ++ show body
  show (Declare toplevel gens) = "def " ++ show gens ++ show toplevel

instance Show Pattern where
  show (PVariable x _) = x
  show (PLiteral l) = show l
  show (PConstructor x ps) = x ++ " " ++ unwords (map show ps)
  show PWildcard = "_"
  show (PList ps) = "[" ++ unwords (map show ps) ++ "]"
  show (PStructure ps) = "{" ++ unwords (map (\(x, p) -> x ++ " = " ++ show p) ps) ++ "}"

instance Show Expression where
  show (Variable x _) = x
  show (Literal l) = show l
  show (Application f xs) = show f ++ "(" ++ intercalate ", " (map show xs) ++ ")"
  show (Lambda xs e) = "fun(" ++ unwords (map show xs) ++ ") " ++ show e
  show (Let (x :@ t) e1 e2) = "let " ++ x ++ ": " ++ show t ++ " = " ++ show e1 ++ " in " ++ show e2
  show (Match e ps) = "match " ++ show e ++ " { " ++ unwords (map (\(p, e') -> show p ++ " = " ++ show e') ps) ++ " }"
  show (Sequence es) = "\n{" ++ unlines (map (("  "++) . show) es) ++ "}"
  show (List es) = "[" ++ unwords (map show es) ++ "]"
  show (ListAccess e1 e2) = show e1 ++ "[" ++ show e2 ++ "]"
  show (While e1 e2) = "while " ++ show e1 ++ " " ++ show e2
  show (For (x :@ t) e1 e3) = "for " ++ x ++ ": " ++ show t ++ " = " ++ show e1 ++ " " ++ show e3
  show (If e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
  show (Update x e) = show x ++ " = " ++ show e
  show (Return e) = "return " ++ show e
  show (Binary op e1 e2) = show e1 ++ " " ++ op ++ " " ++ show e2
  show (Structure xs) = "{" ++ unwords (map (\(x, e) -> x ++ " = " ++ show e) xs) ++ "}"
  show (StructureAccess e x) = show e ++ "." ++ x
  show _ = "?"

instance Show Updated where
  show (VariableUpdate x _) = x
  show (ListUpdate x y) = show x ++ "[" ++ show y ++ "]"
  show (StructureUpdate x y) = show x ++ "." ++ y
