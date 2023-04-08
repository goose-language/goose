module Language.Goose.CLang.Definition.IR where
import Language.Goose.CST.Literal

data IRToplevel
  = IRFunction String [String] [IRStatement]
  | IRDeclaration String IRExpression
  | IRExtern String CType
  | IRDeclare String [CType] CType
  | IRStruct String [IRStructField]
  deriving Eq

data IRStructField 
  = IRStructField String CType
  | IRStructUnion String [IRStructField]
  | IRStructStruct String [IRStructField]
  deriving Eq

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
  deriving Eq

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
  | IRReference IRExpression
  | IRDictAccess IRExpression String
  | IRIn IRExpression String
  deriving Eq

type CType = String