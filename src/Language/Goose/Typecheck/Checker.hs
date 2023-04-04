{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Language.Goose.Typecheck.Checker where
import Language.Goose.Typecheck.Definition.Monad
import Language.Goose.Typecheck.Definition.Type
import Language.Goose.Typecheck.Modules.Substitution
import Language.Goose.Typecheck.Modules.Apply
import Language.Goose.Typecheck.Modules.Parser
import Language.Goose.Typecheck.ConstraintSolver

import qualified Language.Goose.CST.Expression as C
import qualified Language.Goose.CST.Annoted as C
import qualified Language.Goose.CST.Located as C
import qualified Language.Goose.CST.Literal as C
import qualified Language.Goose.CST.Modules.Declaration as D
import qualified Language.Goose.Typecheck.Definition.AST as A

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Monad.Except as E
import qualified Control.Monad as CM
import qualified Data.Bifunctor as BF
import qualified Control.Monad.State as ST
import qualified Data.List as L

generalize :: Environment -> Type -> Scheme
generalize env t = Forall vars t
  where vars = S.toList (free t S.\\ free env)

unify :: MonadChecker m => Constraint -> m ()
unify c = ST.modify (\s -> s { constraints = constraints s ++ [c] })

type Infer f m f' = MonadChecker m => C.Located f -> m (Type, f')

withVariable :: MonadChecker m => (String, Scheme) -> m a -> m a
withVariable (name, scheme) = local (BF.first $ \env -> do
  let env' = M.delete name env
  M.insert name scheme env')

withVariables :: MonadChecker m => [(String, Scheme)] -> m a -> m a
withVariables xs = local (BF.first $ \env -> do
  let env' = foldr (M.delete . fst) env xs
  foldr (uncurry M.insert) env' xs)

inferExpression :: Infer C.Expression m A.Expression
inferExpression (C.Located _ (C.Literal lit)) = do
  (t, l) <- inferLiteral lit
  return (t, A.Literal l)
inferExpression (C.Located pos (C.Dereference e)) = do
  tv <- fresh
  (t, e') <- inferExpression e
  unify (t :~: Mutable tv, pos)
  return (tv, e')
inferExpression (C.Located pos (C.Variable (D.Simple name))) = do
  env <- ask
  case M.lookup name (fst env) of
    Just scheme -> do
      t <- instantiate scheme
      return (t, A.Variable name t)
    Nothing ->  case M.lookup name (snd env) of
      Just scheme -> do
        t <- instantiate scheme
        return (t, A.Variable name t)
      Nothing -> E.throwError ("Variable not found: " ++ name, Nothing, pos)
inferExpression (C.Located pos (C.Application f xs)) = do
  tv <- fresh
  (t1, e1') <- local' $ inferExpression f
  (t2, args) <- L.unzip <$> CM.forM xs (local' . inferExpression)
  unify (t1 :~: (t2 :-> tv), pos)
  return (tv, A.Application e1' args)
inferExpression (C.Located pos (C.Let name value body)) = do
  tv <- fresh
  (t', e') <- withVariable (name, Forall [] tv) $ inferExpression value

  unify (tv :~: t', pos)
  let scheme = Forall [] tv
  (t'', e'') <- withVariable (name, scheme) $ inferExpression body
  ST.modify $ \s' -> s' { variables = M.insert name scheme (variables s') }
  return (t'', A.Let (name C.:@ tv) e' e'')
inferExpression (C.Located _ (C.Sequence exprs)) = do
  (t1, es) <- L.unzip <$> local' (CM.forM exprs inferExpression)
  return (makeType t1, A.Sequence es)
inferExpression (C.Located pos (C.List exprs)) = do
  tv <- fresh
  (es') <- CM.forM exprs (\e -> do
    (t, e') <- local' $ inferExpression e
    unify (t :~: tv, pos)
    return e')
  return (TList tv, A.List es')
inferExpression (C.Located pos (C.ListAccess list index)) = do
  tv <- fresh
  (t1, e1') <- local' $ inferExpression list
  (t2, e2') <- local' $ inferExpression index
  unify (t1 :~: TList tv, pos)
  unify (t2 :~: Int, pos)
  return (tv, A.ListAccess e1' e2')
inferExpression (C.Located pos (C.Binary op e1 e2)) = do
  (t, e) <- inferExpression (C.Located pos (C.Application (C.Located pos (C.Variable (D.Simple op))) [e1, e2]))
  case e of
    A.Application _ [e1', e2'] -> return (t, A.Binary op e1' e2')
    _ -> E.throwError ("Internal error: inferExpression", Nothing, pos)
inferExpression (C.Located pos (C.While cond body)) = do
  (t1, e1') <- local' $ inferExpression cond
  (t2, e2') <- (L.unzip <$>) $ local' $ mapM inferExpression body
  unify (t1 :~: Bool, pos)
  return (makeType t2, A.While e1' e2')
inferExpression (C.Located pos (C.For name list body)) = do
  tv <- fresh
  (t1, e1') <- local' $ inferExpression list
  unify (t1 :~: TList tv, pos)
  (t2, e2') <- withVariable (name, Forall [] tv) $ (L.unzip <$>) $ local' $ mapM inferExpression body
  return (makeType t2, A.For (name C.:@ t1) e1' e2')
inferExpression (C.Located pos (C.If cond then' else')) = do
  (t1, e1') <- local' $ inferExpression cond
  (t2, e2') <- local' $ inferExpression then'
  unify (t1 :~: Bool, pos)
  case else' of
    Just e -> do
      (t3, e3') <- local' $ inferExpression e
      unify (t2 :~: t3, pos)
      return (t2, A.If e1' e2' $ Just e3')
    Nothing -> return (t2, A.If e1' e2' Nothing)
inferExpression (C.Located pos (C.Update update expr)) = do
  (t1, e1') <- local' $ inferUpdate update
  (t2, e2') <- local' $ inferExpression expr
  unify (t1 :~: t2, pos)
  return (Void, A.Update e1' e2')
inferExpression (C.Located pos (C.Return expr)) = do
  (t, e') <- local' $ inferExpression expr
  ret <- ST.gets returnType
  unify (t :~: ret, pos)
  return (t, A.Return e')
inferExpression (C.Located _ (C.Structure fields)) = do
  (ts, fs) <- L.unzip <$> CM.forM fields (\(name, expr) -> do
    (t, e') <- local' $ inferExpression expr
    return ((name, t), (name, e')))
  return (TRec ts, A.Structure fs)
inferExpression (C.Located pos (C.StructureAccess struct field)) = do
  tv <- fresh
  (t, e') <- local' $ inferExpression struct
  unify (Field field tv t, pos)
  return (tv, A.StructureAccess e' field)
inferExpression (C.Located _ (C.Lambda args body)) = do
  tvs <- CM.replicateM (length args) fresh
  ret <- ST.gets returnType
  ST.modify $ \s' -> s' { returnType = makeType tvs }
  (t, e') <- withVariables (zip args (map (Forall []) tvs)) $ local' $ inferExpression body
  ST.modify $ \s' -> s' { returnType = ret }
  return (tvs :-> t, A.Lambda (zipWith C.Annoted args tvs) e')
inferExpression (C.Located pos _) = E.throwError ("Invalid expression", Nothing, pos)

inferUpdate :: Infer C.Updated m A.Updated
inferUpdate (C.Located pos (C.VariableUpdate (D.Simple name))) = do
  (env, _) <- ask
  case M.lookup name env of
    Nothing -> E.throwError ("Unbound variable: " ++ name, Nothing, pos)
    Just scheme -> do
      t <- instantiate scheme
      return (t, A.VariableUpdate name t)
inferUpdate (C.Located pos (C.ListUpdate up index)) = do
  tv <- fresh
  (t, e') <- local' $ inferUpdate up
  unify (t :~: TList tv, pos)
  (t', e'') <- local' $ inferExpression index
  unify (t' :~: Int, pos)
  return (tv, A.ListUpdate e' e'')
inferUpdate (C.Located pos (C.StructureUpdate up field)) = do
  tv <- fresh
  (t, e') <- local' $ inferUpdate up
  unify (Field field tv t, pos)
  return (tv, A.StructureUpdate e' field)
inferUpdate (C.Located pos _) = E.throwError ("Unimplemented", Nothing, pos)

inferLiteral :: MonadChecker m => C.Literal -> m (Type, C.Literal)
inferLiteral (C.Int i) = return (Int, C.Int i)
inferLiteral (C.Float f) = return (Float, C.Float f)
inferLiteral (C.Char c) = return (Char, C.Char c)
inferLiteral (C.String s) = return (TList Char, C.String s)
inferLiteral (C.Bool b) = return (Bool, C.Bool b)
inferLiteral C.Unit = return (Void , C.Unit)

makeType :: [Type] -> Type
makeType [] = Void
makeType xs = last xs

inferToplevel :: Infer C.Toplevel m A.Toplevel
inferToplevel (C.Located pos (C.Function name args expr)) = do
  ret <- fresh
  ST.modify $ \s' -> s' { returnType = ret }

  argsTypes <- mapM (const fresh) args

  let funTy = argsTypes :-> ret
  let args' = (name, Forall [] funTy) : zip args (map (Forall []) argsTypes)

  (t, e') <- withVariables args' $ inferExpression expr
  unify (t :~: ret, pos)

  csts <- ST.gets constraints
  s <- solve csts
  ST.modify $ \s' -> s' { constraints = [] }

  env <- ask
  let scheme = generalize env (apply s funTy)
  ST.modify $ \s' -> s' { variables = M.insert name scheme (variables s') }
  return (Void, apply s $ A.Function name (zipWith C.Annoted args argsTypes) ret e')
inferToplevel (C.Located _ (C.Public tl)) = inferToplevel tl
inferToplevel (C.Located pos (C.Extern name generics decl ret)) = do
  genericsTy <- mapM (const fresh) generics
  let env = M.fromList $ zip generics genericsTy
  
  decl' <- mapM (flip toWithEnv env) decl
  ret' <- toWithEnv ret env

  generics' <- mapM (\case 
      TVar i -> return i
      _ -> E.throwError ("Invalid generic", Nothing, pos)) genericsTy

  let scheme = Forall generics' $ decl' :-> ret'
  ST.modify $ \s -> s { variables = M.insert name scheme (variables s) }
  return (Void, A.Extern (C.Annoted name (decl' :-> ret')))
inferToplevel (C.Located pos (C.Declare name gens args ret)) = do
  generics <- mapM (const fresh) gens
  let env = M.fromList $ zip gens generics
  args' <- mapM (flip toWithEnv env) args
  ret' <- toWithEnv ret env

  gens' <- mapM (\case 
      TVar i -> return i
      _ -> E.throwError ("Invalid generic", Nothing, pos)) generics

  let ty = if null args' then ret' else args' :-> ret'

  let scheme = Forall gens' ty
  ST.modify $ \s -> s { variables = M.insert name scheme (variables s) }
  return (Void, A.Declare generics (C.Annoted name ty))
inferToplevel (C.Located pos _) = E.throwError ("Unimplemented", Nothing, pos)

performInfer :: Monad m => [C.Located C.Toplevel] -> m (Either (String, Maybe String, C.Position) [A.Toplevel])
performInfer toplevels = E.runExceptT $ ST.evalStateT (mapM ((snd <$>) . inferToplevel) toplevels) (CheckerState 0 functions mempty [] Void)

functions :: M.Map String Scheme
functions = M.fromList [
    ("&&", Forall [] $ [Bool, Bool] :-> Bool),
    ("||", Forall [] $ [Bool, Bool] :-> Bool),
    ("!", Forall [] $ [Bool] :-> Bool),
    ("==", Forall [1] $ [TVar 1, TVar 1] :-> Bool),
    ("!=", Forall [1] $ [TVar 1, TVar 1] :-> Bool),
    ("<", Forall [1] $ [TVar 1, TVar 1] :-> Bool),
    (">", Forall [1] $ [TVar 1, TVar 1] :-> Bool),
    ("<=", Forall [1] $ [TVar 1, TVar 1] :-> Bool),
    (">=", Forall [1] $ [TVar 1, TVar 1] :-> Bool),
    ("*", Forall [1] $ [TVar 1, TVar 1] :-> TVar 1),
    ("/", Forall [1] $ [TVar 1, TVar 1] :-> TVar 1),
    ("-", Forall [1] $ [TVar 1, TVar 1] :-> TVar 1),
    ("()", Forall [] $ Void),
    ("+", Forall [1] $ [TVar 1, TVar 1] :-> TVar 1)
  ] 