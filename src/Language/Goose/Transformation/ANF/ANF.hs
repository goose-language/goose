{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Language.Goose.Transformation.ANF.ANF where
import qualified Language.Goose.Transformation.ANF.AST as A
import qualified Language.Goose.Typecheck.Definition.AST as T
import qualified Language.Goose.Typecheck.Definition.Type as TY
import qualified Language.Goose.CST.Annoted as C
import qualified Language.Goose.CST.Literal as C
import qualified Control.Monad.RWS as RWS
import qualified Data.List as L

import Language.Goose.Transformation.ANF.Monad

v :: TY.Type
v = TY.Mutable TY.Void

anfTL :: MonadANF m => T.Toplevel -> m ([(String, A.ANFExpression)], A.ANFDefinition)
anfTL (T.Function name args _ expr) = do
  (lets, expr') <- anfStmt expr
  case name of
    "main" -> return $ ([], A.DFunction name [] (createLet lets ++ expr'))
    _ -> do
      expr <- case last expr' of
        A.SReturn _ -> return expr'
        A.SExpression e -> return $ init expr' ++ [A.SReturn e]
        _ -> return $ expr' ++ [A.SReturn $ A.ELiteral C.Unit]
      return $ ([], A.DFunction name (map C.annotedName args) (createLet lets ++ expr))
anfTL (T.Declaration name _ expr) = do
  (lets, expr') <- anfExpr expr
  return $ (lets, A.DDeclaration name expr')
anfTL (T.Declare _ name) = return ([], A.DDeclare name)

anfStmt :: MonadANF m => T.Expression -> m ([(String, A.ANFExpression)], [A.ANFStatement])
anfStmt (T.Let (C.Annoted name _) expr body) = do
  (lets, expr) <- anfExpr expr
  (lets', expr') <- anfExpr body
  return $ (lets ++ (name, expr) : lets', if body == T.Literal C.Unit then [] else [A.SExpression expr'])
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
  case name of
    A.EVariable _ _ -> return $ (lets ++ lets', [A.SUpdate name expr])
    _ -> do
      n <- fresh
      return $ (lets' ++ [(n, name)] ++ lets, [A.SUpdate (A.EVariable n v) expr])
anfStmt (T.Sequence exprs) = do
  (lets, exprs) <- unzip <$> mapM anfStmt exprs
  return $ ([], concat $ zipWith (\let' expr -> createLet let' ++ expr) lets exprs)
anfStmt (T.Match expr cases) = do
  (lets, expr) <- anfExpr expr
  cases <- mapM (\(pattern, body) -> do
    (lets, body) <- anfStmt body
    return (pattern, createLet lets ++ body)) cases
  return $ (lets, [A.SMatch expr cases])
anfStmt e = do
  (lets, e) <- anfExpr e
  return $ (lets, [A.SExpression e])

anfExpr :: MonadANF m => T.Expression -> m ([(String, A.ANFExpression)], A.ANFExpression)
anfExpr (T.Literal lit) = return ([], A.ELiteral lit)
anfExpr (T.Variable name t) = return ([], A.EVariable name t)
-- anfExpr (T.Application z@(T.Application _ _) args') = do
--   n <- fresh
--   (lets, z) <- anfExpr z
--   (lets', args') <- unzip <$> mapM anfExpr args'
--   return $ (lets ++ [(n, z)] ++ concat lets', A.EApplication (A.EVariable n) args')
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
  return $ ([], A.ELambda (map C.annotedName args) (createLet lets ++ [A.SReturn e]))
anfExpr (T.Lambda args (T.Sequence exprs)) = do
  (lets, exprs) <- unzip <$> mapM anfStmt exprs
  return $ ([], A.ELambda (map C.annotedName args) (createLet (concat lets) ++ concat exprs))
anfExpr (T.Lambda args body) = do
  (lets, e) <- anfStmt body
  let e' = case last e of
        A.SReturn _ -> createLet lets ++ e
        A.SExpression e' -> createLet lets ++ init e ++ [A.SReturn e']
        _ -> createLet lets ++ e
  return $ ([], A.ELambda (map C.annotedName args) e')
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
  case updated of
    A.EVariable _ _ -> return $ (lets ++ lets', A.EUpdate updated expr)
    _ -> do
      n <- fresh
      return $ (lets' ++ [(n, updated)] ++ lets, A.EUpdate (A.EVariable n v) expr)
anfExpr (T.Structure fields) = do
  (lets, fields) <- unzip <$> mapM (\(name, expr) -> do
    (lets, expr) <- anfExpr expr
    return (lets, (name, expr))) fields
  return $ (concat lets, A.EStructure fields)
anfExpr (T.StructureAccess struct field) = do
  (lets, struct) <- anfExpr struct
  return $ (lets, A.EStructAccess struct field)
anfExpr (T.Match expr cases) = do
  (lets, expr) <- anfExpr expr
  cases <- mapM (\(pattern, body) -> do
    let body' = case body of
          T.Return e -> T.Return e
          e -> T.Return e
    (lets, body) <- anfStmt body'
    return (pattern, createLet lets ++ body)) cases
  return $ (lets, embed $ [A.SMatch expr cases])
anfExpr (T.Return e) = anfExpr e
anfExpr (T.For name list body) = do
  (lets, list) <- anfExpr list
  (lets'', body) <- unzip <$> mapM anfStmt body
  return $ ([], embed $ createLet lets ++ [A.SFor (C.annotedName name) list (createLet (concat lets'') ++ concat body)])
anfExpr (T.While cond body) = do
  (lets, cond) <- anfExpr cond
  (lets', body) <- unzip <$> mapM anfStmt body
  return $ ([], embed $ createLet lets ++ [A.SWhile cond (createLet (concat lets') ++ concat body)])
anfExpr (T.Mutable e) = do
  (lets, e) <- anfExpr e
  return $ (lets, A.EMutable e)
anfExpr (T.Dereference e) = do
  (lets, e) <- anfExpr e
  return $ (lets, A.EDereference e)


anfUpdated :: MonadANF m => T.Updated -> m ([(String, A.ANFExpression)], A.ANFExpression)
anfUpdated (T.VariableUpdate name t) = return ([], A.EVariable name t)
anfUpdated (T.ListUpdate updated index) = do
  (lets, updated) <- anfUpdated updated
  (lets', index) <- anfExpr index
  return $ (lets ++ lets', A.EListAccess updated index)
anfUpdated (T.StructureUpdate updated field) = do
  (lets, updated) <- anfUpdated updated
  return $ (lets, A.EStructAccess updated field)

embed :: [A.ANFStatement] -> A.ANFExpression
embed [] = A.ELiteral (C.Unit)
embed ast = A.EApplication (A.ELambda [] ast) []

runANF :: Monad m => [T.Toplevel] -> m [A.ANFDefinition]
runANF xs = do
  (lets, xs) <- L.unzip . fst <$> RWS.evalRWST (mapM anfTL xs) [] 0
  return (createLetTL (concat lets) ++ xs)
