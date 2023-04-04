module Language.Goose.Transformation.ANF.AST where
import Language.Goose.CST.Literal ( Literal )
import Language.Goose.CST.Annoted ( Annoted )
import Language.Goose.Typecheck.Definition.Type

data ANFUpdated 
  = UVariable String
  | UListAccess ANFUpdated ANFExpression
  | UStructAccess ANFUpdated String
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data ANFDefinition
  = DFunction String [String] [ANFStatement]
  | DDeclaration (String) ANFExpression
  | DExtern (Annoted Type)
  | DDeclare (Annoted Type)
  deriving (Eq, Show)