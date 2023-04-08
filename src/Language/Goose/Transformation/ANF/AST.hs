module Language.Goose.Transformation.ANF.AST where
import Language.Goose.CST.Literal ( Literal )
import Language.Goose.CST.Annoted ( Annoted )
import Language.Goose.Typecheck.Definition.Type
import qualified Language.Goose.Typecheck.Definition.AST as A
import Data.List

data ANFUpdated 
  = UVariable String
  | UListAccess ANFUpdated ANFExpression
  | UStructAccess ANFUpdated String
  deriving Eq

instance Show ANFUpdated where
  show (UVariable name) = name
  show (UListAccess updated expr) = show updated ++ "[" ++ show expr ++ "]"
  show (UStructAccess updated name) = show updated ++ "." ++ name

data ANFExpression 
  = EVariable String
  | ELiteral Literal
  | EApplication ANFExpression [ANFExpression]
  | EBinary String ANFExpression ANFExpression
  | EUnary String ANFExpression
  | EIf ANFExpression ANFExpression (Maybe ANFExpression)
  | ELambda [String] [ANFStatement]
  | EList [ANFExpression]
  | EListAccess ANFExpression ANFExpression
  | EUpdate ANFUpdated ANFExpression
  | EStructure [(String, ANFExpression)]
  | EStructAccess ANFExpression String
  deriving Eq

instance Show ANFExpression where
  show (EVariable name) = name
  show (ELiteral literal) = show literal
  show (EApplication expr exprs) = show expr ++ "(" ++ intercalate ", " (map show exprs) ++ ")"
  show (EBinary op expr1 expr2) = show expr1 ++ " " ++ op ++ " " ++ show expr2
  show (EUnary op expr) = op ++ show expr
  show (EIf cond then' else') = "if " ++ show cond ++ " then " ++ show then' ++ " else " ++ show else'
  show (ELambda args body) = "lambda " ++ show args ++ " -> " ++ show body
  show (EList exprs) = "[" ++ show exprs ++ "]"
  show (EListAccess list index) = show list ++ "[" ++ show index ++ "]"
  show (EUpdate updated expr) = show updated ++ " = " ++ show expr
  show (EStructure fields) = "{" ++ intercalate ", " (map (\(n, e) -> n ++ ": " ++ show e) fields) ++ "}"
  show (EStructAccess struct field) = show struct ++ "." ++ field

data ANFStatement
  = SLet String ANFExpression
  | SReturn ANFExpression
  | SIf ANFExpression [ANFStatement] [ANFStatement]
  | SWhile ANFExpression [ANFStatement]
  | SFor String ANFExpression [ANFStatement]
  | SExpression ANFExpression
  | SBlock [ANFStatement]
  | SBreak
  | SUpdate ANFUpdated ANFExpression
  | SContinue
  | SMatch ANFExpression [(A.Pattern, [ANFStatement])]
  deriving Eq

instance Show ANFStatement where
  show (SLet name expr) = "let " ++ name ++ " = " ++ show expr
  show (SReturn expr) = "return " ++ show expr
  show (SIf cond then' else') = "if " ++ show cond ++ " then " ++ show then' ++ " else " ++ show else'
  show (SWhile cond body) = "while " ++ show cond ++ " do " ++ show body
  show (SFor name list body) = "for " ++ name ++ " in " ++ show list ++ " do " ++ show body
  show (SExpression expr) = show expr
  show (SBlock body) = "block " ++ show body
  show (SBreak) = "break"
  show (SUpdate updated expr) = show updated ++ " = " ++ show expr
  show (SContinue) = "continue"
  show (SMatch expr cases) = "match " ++ show expr ++ " with " ++ intercalate " | " (map (\(e, b) -> show e ++ " -> " ++ show b) cases)

data ANFDefinition
  = DFunction String [String] [ANFStatement]
  | DDeclaration (String) ANFExpression
  | DExtern (Annoted Type)
  | DDeclare (Annoted Type)
  deriving Eq

instance Show ANFDefinition where
  show (DFunction name args body) = "function " ++ name ++ "(" ++ show args ++ ") -> " ++ show body
  show (DDeclaration name expr) = "declaration " ++ name ++ " = " ++ show expr
  show (DExtern type') = "extern " ++ show type'
  show (DDeclare type') = "declare " ++ show type'