module Language.Goose.Transformation.BlockRemover where

import Language.Goose.Transformation.ANF.AST
import qualified Control.Arrow as BF

removeBlocks :: [ANFStatement] -> [ANFStatement]
removeBlocks = go
  where go :: [ANFStatement] -> [ANFStatement]
        go [] = []
        go (SBlock stmts : stmts') = go stmts ++ go stmts'
        go (stmt : stmts) = stmt : go stmts

removeToplevel :: [ANFDefinition] -> [ANFDefinition]
removeToplevel = map go
  where go :: ANFDefinition -> ANFDefinition
        go (DFunction name args body) = DFunction name args (removeBlocks body)
        go (DDeclaration name expr) = DDeclaration name (removeExpr expr)
        go (DDeclare name) = DDeclare name

removeStatement :: [ANFStatement] -> [ANFStatement]
removeStatement (SBlock stmts:xs) = removeBlocks stmts ++ removeStatement xs
removeStatement (SIf cond then_ else_:xs) = SIf cond (removeStatement then_) (removeStatement else_) : removeStatement xs
removeStatement (SWhile cond body:xs) = SWhile cond (removeStatement body) : removeStatement xs
removeStatement (SFor name list exprs:xs) = SFor name (removeExpr list) (removeStatement exprs) : removeStatement xs
removeStatement (SReturn expr:xs) = SReturn (removeExpr expr) : removeStatement xs
removeStatement (SUpdate name expr:xs) = SUpdate name (removeExpr expr) : removeStatement xs
removeStatement (SLet name expr:xs) = SLet name (removeExpr expr) : removeStatement xs
removeStatement (SExpression e:xs) = SExpression (removeExpr e) : removeStatement xs
removeStatement (SBreak:_) = []
removeStatement (SContinue:_) = []
removeStatement (SMatch expr cases:xs) = SMatch (removeExpr expr) (map (BF.second removeStatement) cases) : removeStatement xs
removeStatement [] = []

removeExpr :: ANFExpression -> ANFExpression
removeExpr (ELambda args body) = ELambda args (removeBlocks body)
removeExpr (EIf cond then_ else_) = EIf cond (removeExpr then_) (removeExpr <$> else_)
removeExpr (EApplication func args) = EApplication (removeExpr func) (map removeExpr args)
removeExpr (EVariable name v) = EVariable name v
removeExpr (ELiteral l) = ELiteral l
removeExpr (EUpdate name expr) = EUpdate name (removeExpr expr)
removeExpr (EBinary op lhs rhs) = EBinary op (removeExpr lhs) (removeExpr rhs)
removeExpr (EUnary op expr) = EUnary op (removeExpr expr)
removeExpr (EList exprs) = EList (map removeExpr exprs)
removeExpr (EListAccess list index) = EListAccess (removeExpr list) (removeExpr index)
removeExpr (EStructure fields) = EStructure (map (BF.second removeExpr) fields)
removeExpr (EStructAccess struct field) = EStructAccess (removeExpr struct) field
removeExpr (EMutable expr) = EMutable (removeExpr expr)
removeExpr (EDereference expr) = EDereference (removeExpr expr)
