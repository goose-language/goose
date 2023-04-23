{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
module Language.Goose.Transformation.Closure.Conversion where
import Language.Goose.Transformation.ANF.AST
import Language.Goose.CST.Annoted
import Language.Goose.Transformation.Closure.Free (Free(free))
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Bifunctor as B
import Language.Goose.Typecheck.Checker
import Control.Monad.State
import Data.List (nub)
import qualified Language.Goose.CST.Literal as C
import Language.Goose.Typecheck.Definition.Type
import Control.Monad.RWS hiding (local)

type MonadClosure m = (MonadRWS () [ANFDefinition] (Int, S.Set String) m)

fresh :: MonadClosure m => m Int
fresh = do
  (i, e) <- get
  put (i + 1, e)
  return i

freshLambda :: MonadClosure m => m String
freshLambda = do
  i <- fresh
  return $ "$$l" ++ show i

freshName :: MonadClosure m => m String
freshName = do
  i <- fresh
  return $ "$$a" ++ show i

isExcluded :: MonadClosure m => String -> m Bool
isExcluded name = do
  (_, e) <- get
  return $ S.member name e

addExcluded :: MonadClosure m => String -> m ()
addExcluded name = do
  (i, e) <- get
  put (i, S.insert name e)

local :: MonadClosure m => m a -> m a
local m = do
  s <- get
  r <- m
  cnt <- gets fst
  put (cnt, snd s)
  return r

removeExcluded :: MonadClosure m => String -> m ()
removeExcluded name = do
  (i, e) <- get
  put (i, S.delete name e)

convertToplevel :: MonadClosure m => ANFDefinition -> m ANFDefinition
convertToplevel (DDeclaration name (ELambda args body)) = do
  addExcluded name
  expr' <- concat <$> mapM convertStatement body
  return $ DFunction name ("env" : args) expr'
convertToplevel (DDeclaration name expr) = do
  (expr', _) <- convertExpression expr
  return $ DDeclaration name expr'
convertToplevel (DFunction name args body) = do
  addExcluded name
  expr' <- concat <$> mapM convertStatement body
  return $ DFunction name (if name == "main" then [] else "env" : args) expr'
convertToplevel (DDeclare (Annoted name ty)) = do
  addExcluded name
  return $ DDeclare (Annoted name ty)

v :: Type
v = TId "Value"

substExpr :: ANFExpression -> String -> ANFExpression -> ANFExpression
substExpr z@(ELambda args body) name expr = if name `elem` args then z else ELambda args (map (\e -> subst e name expr) body)
substExpr z@(EVariable name' _) name expr = if name == name' then expr else z
substExpr (EApplication e1 e2) name expr = EApplication (substExpr e1 name expr) (map (\e -> substExpr e name expr) e2)
substExpr (EStructure fields) name expr = EStructure $ map (\(n, e) -> (n, substExpr e name expr)) fields
substExpr (EStructAccess e n) name expr = EStructAccess (substExpr e name expr) n
substExpr (EIf e1 e2 e3) name expr = EIf (substExpr e1 name expr) (substExpr e2 name expr) (substExpr <$> e3 <*> pure name <*> pure expr)
substExpr z@(ELiteral _) _ _ = z
substExpr (EBinary op e1 e2) name expr = EBinary op (substExpr e1 name expr) (substExpr e2 name expr)
substExpr (EUnary op e) name expr = EUnary op (substExpr e name expr)
substExpr (EList exprs) name expr = EList $ map (\e -> substExpr e name expr) exprs
substExpr (EListAccess e1 e2) name expr = EListAccess (substExpr e1 name expr) (substExpr e2 name expr)
substExpr (EUpdate e1 e2) name expr = EUpdate (substExpr e1 name expr) (substExpr e2 name expr)
substExpr (EMutable e) name expr = EMutable $ substExpr e name expr
substExpr (EDereference e) name expr = EDereference $ substExpr e name expr

subst :: ANFStatement -> String -> ANFExpression -> ANFStatement
subst (SExpression e) name expr = SExpression $ substExpr e name expr
subst (SUpdate e1 e2) name expr = SUpdate (substExpr e1 name expr) (substExpr e2 name expr)
subst (SReturn e) name expr = SReturn $ substExpr e name expr
subst (SIf e1 e2 e3) name expr = SIf (substExpr e1 name expr) (map (\e -> subst e name expr) e2) (subst <$> e3 <*> pure name <*> pure expr)
subst (SWhile e1 e2) name expr = SWhile (substExpr e1 name expr) (map (\e -> subst e name expr) e2)
subst (SLet name' e) name expr = if name == name' then SLet name' e else SLet name' (substExpr e name expr)
subst (SFor name list exprs) name' expr = SFor name (substExpr list name' expr) $ if name == name' then exprs else map (\e -> subst e name expr) exprs
subst (SBlock exprs) name expr = SBlock $substBlock exprs name expr
subst SBreak _ _ = SBreak
subst SContinue _ _ = SContinue
subst (SMatch e cases) name expr = SMatch (substExpr e name expr) $ map (\(p, e') -> if name `S.member` free p then (p, e') else (p, map (\e'' -> subst e'' name expr) e')) cases

substBlock :: [ANFStatement] -> String -> ANFExpression -> [ANFStatement]
substBlock (SLet name e:rest) name' expr = if name == name' then SLet name e : rest else SLet name (substExpr e name' expr) : substBlock rest name' expr
substBlock (SBlock exprs:rest) name expr = SBlock (substBlock exprs name expr) : substBlock rest name expr
substBlock (e:rest) name expr = subst e name expr : substBlock rest name expr
substBlock [] _ _ = []

closureConvert :: MonadClosure m => ANFExpression -> String -> m ANFExpression
closureConvert (ELambda args expr) name = do
  lambdaName <- freshLambda
  let funTy = (v : map (const v) args) :-> v
  -- traceShowM expr
  let lambdaBody = if name == "" then expr else map (\e -> subst e name (EVariable lambdaName funTy)) expr
  -- traceShowM (name, lambdaBody)
  addExcluded lambdaName
  (_, excluded) <- get
  let freed = free (ELambda args lambdaBody)
  let env = S.filter (`notElem` excluded) freed
  let declarations = nub $ map (\n -> SLet n (EStructAccess (EVariable "env" v) n)) $ S.toList env
  expr' <- mapM convertStatement lambdaBody
  -- traceShowM expr'

  let body = case expr' of stmts -> declarations ++ concat stmts

  tell [DFunction lambdaName ("env" : args) body]

  let envStruct = map (\n -> if n == name then (n, EStructure [("$$func", ELiteral C.Unit), ("env", ELiteral C.Unit)]) else (n, EVariable n v)) $ S.toList env

  return $ EStructure [("$$func", EApplication (EVariable "makeLambda" v) [EVariable lambdaName funTy]), ("env", EStructure envStruct)]
closureConvert _ _ = undefined

convertStatement :: MonadClosure m => ANFStatement -> m [ANFStatement]
convertStatement (SExpression e) = do
  (e', stmts) <- convertExpression e
  return $ stmts ++ [SExpression e']
convertStatement (SLet name z@ELambda {}) = do
  expr' <- closureConvert z name
  removeExcluded name
  return [SLet name expr']
convertStatement (SLet name e) = do
  (expr', stmts) <- convertExpression e
  removeExcluded name
  return $ stmts ++ [SLet name expr']
convertStatement (SIf cond t f) = do
  (cond', stmts) <- convertExpression cond
  t' <- concat <$> mapM convertStatement t
  f' <- concat <$> mapM convertStatement f
  return $ stmts ++ [SIf cond' t' f']
convertStatement (SWhile cond body) = do
  (cond', stmts) <- convertExpression cond
  body' <- concat <$> mapM convertStatement body
  return $ stmts ++ [SWhile cond' body']
convertStatement (SUpdate n e) = do
  (e', stmts) <- convertExpression e
  return $ stmts ++ [SUpdate n e']
convertStatement (SReturn e) = do
  (e', stmts) <- convertExpression e
  return $ stmts ++ [SReturn e']
convertStatement (SFor name list body) = do
  (list', stmts) <- convertExpression list
  body' <- concat <$> mapM convertStatement body
  return $ stmts ++ [SFor name list' body']
convertStatement (SBlock xs) = local $ concat <$> mapM convertStatement xs
convertStatement SBreak = return [SBreak]
convertStatement SContinue = return [SContinue]
convertStatement (SMatch e cases) = do
  (e', stmts) <- convertExpression e
  cases' <- mapM (\(p, b) -> (,) p . concat <$> mapM convertStatement b) cases
  return $ stmts ++ [SMatch e' cases']

convertExpression :: MonadClosure m => ANFExpression -> m (ANFExpression, [ANFStatement])
convertExpression (EApplication (EVariable x _) args) = do
  excluded <- isExcluded x
  if not excluded
    then do
      (args', stmts) <- unzip <$> mapM convertExpression args
      let call = EApplication (EStructAccess (EStructAccess (EVariable x v) "$$func") "$$fun") (EStructAccess (EVariable x v) "env" : args')
      return (call, concat stmts)
    else do
      (args', stmts) <- unzip <$> mapM convertExpression args
      return (EApplication (EVariable x v) (ELiteral C.Unit : args'), concat stmts)
convertExpression (EApplication f args) = do
  (f', stmts1) <- convertExpression f
  (args', stmts2) <- unzip <$> mapM convertExpression args
  n <- freshName
  let decl = SLet n f'
  let var = EVariable n v
  return (EApplication (EStructAccess (EStructAccess var "$$func") "$$fun") (EStructAccess var "env" : args'), stmts1 ++ concat stmts2 ++ [decl])
convertExpression (EIf cond t f) = do
  (cond', stmts1) <- convertExpression cond
  (t', stmts2) <- convertExpression t
  (f', stmts3) <- case f of
    Just f' -> convertExpression f'
    Nothing -> return (ELiteral C.Unit, [])
  return (EIf cond' t' (Just f'), stmts1 ++ stmts2 ++ stmts3)
convertExpression (ELiteral x) = return (ELiteral x, [])
convertExpression (EList xs) = B.bimap EList concat <$> (unzip <$> mapM convertExpression xs)
convertExpression z@ELambda {} = (,[]) <$> closureConvert z ""
convertExpression (EListAccess arr idx) = do
  (arr', stmts1) <- convertExpression arr
  (idx', stmts2) <- convertExpression idx
  return (EListAccess arr' idx', stmts1 ++ stmts2)
convertExpression (EStructAccess str name) = do
  (str', stmts) <- convertExpression str
  return (EStructAccess str' name, stmts)
convertExpression (EStructure xs) = do
  (xs', stmts) <- B.second unzip . unzip <$> mapM (\(n, e) -> (n,) <$> convertExpression e) xs
  return (EStructure (zip xs' $ fst stmts), concat $ snd stmts)
convertExpression (EVariable x t) = do
  case t of
    _ :-> _ -> do
      excluded <- isExcluded x
      if not excluded
        then return (EVariable x v, [])
        else return (EStructure [("$$func", EApplication (EVariable "makeLambda" v) [EVariable x v]), ("env", ELiteral C.Unit)], [])
    _ -> return (EVariable x v, [])
convertExpression (EBinary op e1 e2) = do
  (e1', stmts1) <- convertExpression e1
  (e2', stmts2) <- convertExpression e2
  return (EBinary op e1' e2', stmts1 ++ stmts2)
convertExpression (EUnary op e) = do
  (e', stmts) <- convertExpression e
  return (EUnary op e', stmts)
convertExpression (EUpdate updated e) = do
  (e', stmts) <- convertExpression e
  return (EUpdate updated e', stmts)
convertExpression (EMutable e) = do
  (e', stmts) <- convertExpression e
  return (EMutable e', stmts)
convertExpression (EDereference e) = do
  (e', stmts) <- convertExpression e
  return (EDereference e', stmts)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

runClosureConversion :: [ANFDefinition] -> [ANFDefinition]
runClosureConversion xs = do
  fst3 $ foldl (\(acc, i, excl) x -> do
    let (str, (i', excluded), defs) = runRWS (convertToplevel x) () (i, excl)
    (acc ++ defs ++ [str], i', excl `S.union` excluded)) ([], 0, S.fromList $ M.keys functions) xs
