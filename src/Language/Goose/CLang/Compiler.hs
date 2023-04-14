module Language.Goose.CLang.Compiler where
import Language.Goose.Transformation.ANF.AST
import Language.Goose.CST.Annoted
import Language.Goose.Typecheck.Definition.Type
import Language.Goose.CLang.Definition.IR
import Data.Maybe
import Language.Goose.CST.Literal
import Language.Goose.CLang.Definition.Generation
import Language.Goose.CLang.Pattern
import Data.Bifunctor
import Data.List

compileToplevel :: ANFDefinition -> Maybe IRToplevel
compileToplevel (DFunction name args body) = Just $ IRFunction name args (map compileStatement body)
compileToplevel (DDeclaration name e) = Just $ IRDeclaration name (compileExpression e)
compileToplevel (DExtern (Annoted name _)) = Just $ IRExtern name "int"
compileToplevel (DDeclare (Annoted _ _)) = Nothing
compileToplevel (DInternDeclare name) = Just $ IRDeclare name [] rttiName 

from :: Type -> CType
from _ = rttiName

createIfSequence :: [IRStatement]-> IRStatement
createIfSequence [] = error "Empty if sequence"
createIfSequence [x] = x
createIfSequence (IRIf cond then':xs) = IRIfElse cond then' [createIfSequence xs]
createIfSequence _ = error "Invalid if sequence"

compileStatement :: ANFStatement -> IRStatement
compileStatement (SReturn e) = IRReturn (compileExpression e)
compileStatement (SIf e t f) = IRIfElse (compileExpression e) (map compileStatement t) (map compileStatement f)
compileStatement (SWhile e s) = IRWhile (compileExpression e) (map compileStatement s)
compileStatement (SFor name e s) = IRFor name (compileExpression e) (map compileStatement s)
compileStatement (SExpression e) = IRExpression (compileExpression e)
compileStatement (SBlock s) = IRBlock (map compileStatement s)
compileStatement SBreak = IRBreak
compileStatement SContinue = IRContinue
compileStatement (SLet name e) = IRDeclarationStatement name (compileExpression e)
compileStatement (SUpdate u e) = IRUpdate (compileExpression u) (compileExpression e)
compileStatement (SMatch e cases) = do
  let x = compileExpression e
  let decl = IRDeclarationStatement "$$match" x
  let xs = map (\(p, b) -> do
          let b' = map compileStatement b
          compileCase p (IRVariable "$$match") b') cases
  IRBlock [
      decl,
      createIfSequence xs
    ]


compileExpression :: ANFExpression -> IRExpression
compileExpression (EVariable name _) = IRVariable name
compileExpression (ELiteral l) = IRLiteral l
compileExpression (EApplication (EVariable "makeLambda" _) [lambda]) = IRApplication (IRVariable "makeLambda") [compileExpression lambda]
compileExpression (EApplication (EVariable "free" _) [value]) = IRApplication (IRVariable "free") [compileExpression value]
compileExpression (EApplication e args) = IRApplication (compileExpression e) [IRList (map compileExpression args)]
compileExpression (EBinary op e1 e2) = case op of
  "+" -> IRApplication (IRVariable "add") [compileExpression e1, compileExpression e2]
  "-" -> IRApplication (IRVariable "subtract") [compileExpression e1, compileExpression e2]
  "*" -> IRApplication (IRVariable "multiply") [compileExpression e1, compileExpression e2]
  "/" -> IRApplication (IRVariable "divide") [compileExpression e1, compileExpression e2]
  "==" -> IRApplication (IRVariable "eq") [compileExpression e1, compileExpression e2]
  "!=" -> IRApplication (IRVariable "neq") [compileExpression e1, compileExpression e2]
  "<" -> IRApplication (IRVariable "lt") [compileExpression e1, compileExpression e2]
  ">" -> IRApplication (IRVariable "gt") [compileExpression e1, compileExpression e2]
  "<=" -> IRApplication (IRVariable "lte") [compileExpression e1, compileExpression e2]
  ">=" -> IRApplication (IRVariable "gte") [compileExpression e1, compileExpression e2]
  "&&" -> IRBinary "&&" (compileExpression e1) (compileExpression e2)
  "||" -> IRBinary "||" (compileExpression e1) (compileExpression e2)
  _ -> error "Not implemented"
compileExpression (EUnary op e) = IRUnary op (compileExpression e)
compileExpression (EIf e t f) = IRTernary (compileExpression e) (compileExpression t) (fromMaybe (IRLiteral Unit) (fmap compileExpression f))
compileExpression (EList es) = IRList (map compileExpression es)
compileExpression (EListAccess e1 e2) = IRListAccess (compileExpression e1) (compileExpression e2)
compileExpression (ELambda _ _) = error "Not implemented"
compileExpression (EUpdate u e) = IREUpdate (compileExpression u) (compileExpression e)
compileExpression (EStructure fields) = IRDict (map (second compileExpression) fields)
compileExpression (EStructAccess e1 e2) = IRDictAccess (compileExpression e1) e2
compileExpression (EMutable e) = IRApplication (IRVariable "create_mutable") [compileExpression e]
compileExpression (EDereference e) = IRApplication (IRVariable "get_mutable") [compileExpression e]


compile :: [ANFDefinition] -> [IRToplevel]
compile = nub . mapMaybe compileToplevel