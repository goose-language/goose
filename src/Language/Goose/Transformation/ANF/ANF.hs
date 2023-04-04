{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Language.Goose.Transformation.ANF.ANF where
import qualified Language.Goose.Transformation.ANF.AST as A
import qualified Language.Goose.Typecheck.Definition.AST as T
import qualified Language.Goose.CST.Annoted as C
import qualified Language.Goose.CST.Literal as C
import qualified Control.Monad.RWS as RWS
import qualified Data.List as L

import Language.Goose.Transformation.ANF.Monad

anfTL :: MonadANF m => T.Toplevel -> m ([(String, A.ANFExpression)], A.ANFDefinition)
anfTL (T.Function name args _ expr) = do
  (lets, expr') <- anfStmt expr
  return $ ([], A.DFunction name (map C.annotedName args) (createLet lets ++ expr'))
anfTL (T.Declaration name _ expr) = do
  (lets, expr') <- anfExpr expr
  return $ (lets, A.DDeclaration name expr')
anfTL (T.Extern name) = return ([], A.DExtern name)
anfTL (T.Declare _ name) = return ([], A.DDeclare name)

anfStmt :: MonadANF m => T.Expression -> m ([(String, A.ANFExpression)], [A.ANFStatement])
anfStmt (T.Let (C.Annoted name _) expr body) = do
  (lets, expr) <- anfExpr expr
  (lets', expr') <- anfExpr body
  return $ (lets ++ (name, expr) : lets', [A.SExpression expr'])
anfStmt (T.Return expr) = do
  (lets, expr) <- anfExpr expr
  return $ (lets, [A.SReturn expr])
anfStmt (T.If cond then' else') = do
  (lets, cond) <- anfExpr cond
  (lets', then') <- anfStmt then'
  (lets'', else') <- case else' of
    Just else' -> do
      (lets'', else') <- anfStmt else'
      return (lets'', Just else')
    Nothing -> return ([], Nothing)
  return $ (lets, [A.SIf cond (createLet lets' ++ then') (maybe (createLet lets'') (\x -> createLet lets'' ++ x) (else'))])
anfStmt (T.While cond body) = do
  (lets, cond) <- anfExpr cond
  (lets', body) <- unzip <$> mapM anfStmt body
  return $ (lets, [A.SWhile cond (concat $ zipWith (\let' expr -> createLet let' ++ expr) lets' body)])
anfStmt (T.For name expr body) = do
  (lets, expr) <- anfExpr expr
  (lets', body) <- unzip <$> mapM anfStmt body
  return $ (lets, [A.SFor (C.annotedName name) expr (concat $ zipWith (\let' expr -> createLet let' ++ expr) lets' body)])
anfStmt (T.Update name expr) = do
  (lets, expr) <- anfExpr expr
  (lets', name) <- anfUpdated name
  return $ (lets' ++ lets, [A.SUpdate name expr])
anfStmt (T.Sequence exprs) = do
  (lets, exprs) <- unzip <$> mapM anfStmt exprs
  return $ ([], concat $ zipWith (\let' expr -> createLet let' ++ expr) lets exprs)
anfStmt e = do
  (lets, e) <- anfExpr e
  return $ (lets, [A.SExpression e])
  

anfExpr :: MonadANF m => T.Expression -> m ([(String, A.ANFExpression)], A.ANFExpression)
anfExpr (T.Literal lit) = return ([], A.ELiteral lit)
anfExpr (T.Variable name _) = return ([], A.EVariable name)
anfExpr (T.Application func args) = do
  (lets, func) <- anfExpr func
  (lets', args) <- unzip <$> mapM anfExpr args
  return $ (lets ++ concat lets', A.EApplication func args)
anfExpr (T.Binary op left right) = do 
  (lets, left) <- anfExpr left
  (lets', right) <- anfExpr right
  return $ (lets ++ lets', A.EBinary op left right)
anfExpr (T.Let (C.Annoted name _) expr body) = do
  (lets, expr) <- anfExpr expr
  (lets', body) <- anfExpr body
  return $ (lets ++ (name, expr) : lets', body)
anfExpr (T.Sequence exprs) = do
  (lets, exprs) <- unzip <$> mapM anfStmt exprs
  return $ ([], embed $ createLet (concat lets) ++ concat exprs)
anfExpr (T.If cond then' else') = do
  (lets, cond) <- anfExpr cond
  (lets', then') <- anfExpr then'
  (lets'', else') <- case else' of
    Just else' -> do
      (lets'', else') <- anfExpr else'
      return (lets'', Just else')
    Nothing -> return ([], Nothing)
  return $ (lets ++ lets' ++ lets'', A.EIf cond then' else')
anfExpr (T.Lambda args (T.Return e)) = do
  (lets, e) <- anfExpr e
  n <- fresh
  return $ ([(n, A.ELambda (map C.annotedName args) (createLet lets ++ [A.SReturn e]))], A.EVariable n)
anfExpr (T.Lambda args body) = do
  (lets, e) <- anfExpr body
  n <- fresh
  return $ ([(n, A.ELambda (map C.annotedName args) (createLet lets ++ [A.SReturn e]))], A.EVariable n)
anfExpr (T.List exprs) = do
  (lets, exprs) <- unzip <$> mapM anfExpr exprs
  return $ (concat lets, A.EList exprs)
anfExpr (T.ListAccess list index) = do
  (lets, list) <- anfExpr list
  (lets', index) <- anfExpr index
  return $ (lets ++ lets', A.EListAccess list index)
anfExpr (T.Update updated expr) = do
  (lets, expr) <- anfExpr expr
  (lets', updated) <- anfUpdated updated
  return $ (lets' ++ lets, A.EUpdate updated expr)
anfExpr (T.Structure fields) = do
  (lets, fields) <- unzip <$> mapM (\(name, expr) -> do
    (lets, expr) <- anfExpr expr
    return (lets, (name, expr))) fields
  return $ (concat lets, A.EStructure fields)
anfExpr (T.StructureAccess struct field) = do
  (lets, struct) <- anfExpr struct
  return $ (lets, A.EStructAccess struct field)
anfExpr x = error $ "Not implemented: " ++ show x

anfUpdated :: MonadANF m => T.Updated -> m ([(String, A.ANFExpression)], A.ANFUpdated)
anfUpdated (T.VariableUpdate name _) = return ([], A.UVariable name)
anfUpdated (T.ListUpdate updated index) = do
  (lets, updated) <- anfUpdated updated
  (lets', index) <- anfExpr index
  return $ (lets ++ lets', A.UListAccess updated index)
anfUpdated (T.StructureUpdate updated field) = do
  (lets, updated) <- anfUpdated updated
  return $ (lets, A.UStructAccess updated field)

embed :: [A.ANFStatement] -> A.ANFExpression
embed [] = A.ELiteral (C.Unit)
embed ast = A.EApplication (A.ELambda [] ast) []

runANF :: Monad m => [T.Toplevel] -> m [A.ANFDefinition]
runANF xs = do
  (lets, xs) <- L.unzip . fst <$> RWS.evalRWST (mapM anfTL xs) [] 0
  return (createLetTL (concat lets) ++ xs)
