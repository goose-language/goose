module Language.Goose.CLang.Compiler where
import Language.Goose.Transformation.ANF.AST
import Language.Goose.CST.Annoted
import Language.Goose.Typecheck.Definition.Type
import Language.Goose.CLang.Definition.IR
import Data.Maybe
import Language.Goose.CST.Literal
import Language.Goose.CLang.Definition.Generation
import Data.Bifunctor
import Data.List

compileToplevel :: ANFDefinition -> IRToplevel
compileToplevel (DFunction name args body) = IRFunction name args (map compileStatement body)
compileToplevel (DDeclaration name e) = IRDeclaration name (compileExpression e)
compileToplevel (DExtern (Annoted name _)) = IRExtern name "int"
compileToplevel (DDeclare (Annoted name t)) = case t of
  args :-> ret -> IRDeclare name (map from args) (from ret)
  ty -> IRDeclare name [] (from ty)

from :: Type -> CType
from _ = rttiName

compileStatement :: ANFStatement -> IRStatement
compileStatement (SReturn e) = IRReturn (compileExpression e)
compileStatement (SIf e t f) = IRIf (compileExpression e) (map compileStatement t) (map compileStatement f)
compileStatement (SWhile e s) = IRWhile (compileExpression e) (map compileStatement s)
compileStatement (SFor name e s) = IRFor name (compileExpression e) (map compileStatement s)
compileStatement (SExpression e) = IRExpression (compileExpression e)
compileStatement (SBlock s) = IRBlock (map compileStatement s)
compileStatement SBreak = IRBreak
compileStatement SContinue = IRContinue
compileStatement (SLet name e) = IRDeclarationStatement name (compileExpression e)
compileStatement (SUpdate u e) = IRUpdate (compileUpdated u) (compileExpression e)

compileExpression :: ANFExpression -> IRExpression
compileExpression (EVariable name) = IRVariable name
compileExpression (ELiteral l) = IRLiteral l
compileExpression (EApplication (EVariable "makeLambda") [lambda]) = IRApplication (IRVariable "makeLambda") [compileExpression lambda]
compileExpression (EApplication (EVariable "freeValue") [value]) = IRApplication (IRVariable "freeValue") [compileExpression value]
compileExpression (EApplication e args) = IRApplication (compileExpression e) [IRList (map compileExpression args)]
compileExpression (EBinary op e1 e2) = case op of
  "+" -> IRApplication (IRVariable "add") [compileExpression e1, compileExpression e2]
  "-" -> IRApplication (IRVariable "sub") [compileExpression e1, compileExpression e2]
  "*" -> IRApplication (IRVariable "mul") [compileExpression e1, compileExpression e2]
  "/" -> IRApplication (IRVariable "div_") [compileExpression e1, compileExpression e2]
  "==" -> IRApplication (IRVariable "eq") [compileExpression e1, compileExpression e2]
  "!=" -> IRApplication (IRVariable "neq") [compileExpression e1, compileExpression e2]
  "<" -> IRApplication (IRVariable "lt") [compileExpression e1, compileExpression e2]
  ">" -> IRApplication (IRVariable "gt") [compileExpression e1, compileExpression e2]
  "<=" -> IRApplication (IRVariable "lte") [compileExpression e1, compileExpression e2]
  ">=" -> IRApplication (IRVariable "gte") [compileExpression e1, compileExpression e2]
  "&&" -> IRApplication (IRVariable "and") [compileExpression e1, compileExpression e2]
  "||" -> IRApplication (IRVariable "or") [compileExpression e1, compileExpression e2]
  _ -> error "Not implemented"
compileExpression (EUnary op e) = IRUnary op (compileExpression e)
compileExpression (EIf e t f) = IRTernary (compileExpression e) (compileExpression t) (fromMaybe (IRLiteral Unit) (fmap compileExpression f))
compileExpression (EList es) = IRList (map compileExpression es)
compileExpression (EListAccess e1 e2) = IRListAccess (compileExpression e1) (compileExpression e2)
compileExpression (ELambda _ _) = error "Not implemented"
compileExpression (EUpdate u e) = IREUpdate (compileUpdated u) (compileExpression e)
compileExpression (EStructure fields) = IRDict (map (second compileExpression) fields)
compileExpression (EStructAccess e1 e2) = IRDictAccess (compileExpression e1) e2

compileUpdated :: ANFUpdated -> IRExpression
compileUpdated (UVariable name) = IRVariable name
compileUpdated (UListAccess e1 e2) = IRListAccess (compileUpdated e1) (compileExpression e2)
compileUpdated (UStructAccess e1 e2) = IRDictAccess (compileUpdated e1) e2

compile :: [ANFDefinition] -> [IRToplevel]
compile = nub . map compileToplevel