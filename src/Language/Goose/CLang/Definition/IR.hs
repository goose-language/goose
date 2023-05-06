module Language.Goose.CLang.Definition.IR where
import Language.Goose.CST.Literal

data IRToplevel
  = IRFunction String [String] [IRStatement]
  | IRDeclaration String IRExpression
  | IRExtern String CType
  | IRDeclare String (Maybe [CType]) CType
  | IRStruct String [IRStructField]
  | IRGlobalString String String
  deriving (Eq, Show)

data IRStructField 
  = IRStructField String CType
  | IRStructUnion String [IRStructField]
  | IRStructStruct String [IRStructField]
  deriving (Eq, Show)

data IRStatement 
  = IRReturn IRExpression
  | IRIfElse IRExpression [IRStatement] [IRStatement]
  | IRIf IRExpression [IRStatement]
  | IRWhile IRExpression [IRStatement]
  | IRFor String IRExpression [IRStatement]
  | IRExpression IRExpression
  | IRBlock [IRStatement]
  | IRBreak
  | IRContinue
  | IRDeclarationStatement String IRExpression
  | IRUpdate IRExpression IRExpression
  deriving (Eq, Show)

data IRExpression
  = IRVariable String 
  | IRApplication IRExpression [IRExpression]
  | IRBinary String IRExpression IRExpression
  | IRUnary String IRExpression
  | IRLiteral Literal
  | IRList [IRExpression]
  | IRListAccess IRExpression IRExpression
  | IRTernary IRExpression IRExpression IRExpression
  | IREUpdate IRExpression IRExpression
  | IRDict [(String, IRExpression)]
  | IRDictAccess IRExpression String
  | IRIn IRExpression String
  | IRIs IRExpression String
  | IRInternDict [(Int, IRExpression)]
  | IRInternDictAccess IRExpression Int
  deriving (Eq, Show)

type CType = String