{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
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
import qualified Language.Goose.CST.Modules.Pattern as P
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

withGenerics :: MonadChecker m => M.Map String Type -> m a -> m a
withGenerics xs m = do
  ST.modify (\s -> s { generics = xs `M.union` generics s })
  a <- m
  ST.modify (\s -> s { generics = M.difference (generics s) xs })
  return a

inferExpression :: Infer C.Expression m A.Expression
inferExpression (C.Located _ (C.Literal lit)) = do
  (t, l) <- inferLiteral lit
  return (t, A.Literal l)
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
inferExpression (C.Located pos (C.Let (C.Annoted name t) value body)) = do
  generics' <- ST.gets generics
  tv <- toWithEnv t generics'
  (t', e') <- withVariable (name, Forall [] tv) $ inferExpression value

  unify (tv :~: t', pos)
  let scheme = Forall [] tv
  (t'', e'') <- withVariable (name, scheme) $ inferExpression body
  ST.modify $ \s' -> s' { variables = M.insert name scheme (variables s') }
  return (t'', A.Let (name C.:@ tv) e' e'')
inferExpression (C.Located pos (C.Sequence exprs)) = do
  ret <- ST.gets returnType
  tv <- fresh
  ST.modify (\s -> s { returnType = tv })
  (t1, es) <- L.unzip <$> local' (CM.forM exprs inferExpression)
  unify (makeType t1 :~: tv, pos)
  ST.modify (\s -> s { returnType = ret })
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
inferExpression (C.Located pos (C.For (C.Annoted name t) list body)) = do
  generics' <- ST.gets generics
  tv <- toWithEnv t generics'
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
inferExpression (C.Located pos (C.Lambda args ret body)) = do
  generics' <- ST.gets generics
  tv <- toWithEnv ret generics'
  tvs <- mapM (\(C.Annoted name t) -> (name,) <$> toWithEnv t generics') args
  ret' <- ST.gets returnType
  ST.modify $ \s' -> s' { returnType = tv }
  (t, e') <- withVariables (zip (map C.annotedName args) (map (Forall [] . snd) tvs)) $ local' $ inferExpression body
  unify (t :~: tv, pos)
  ST.modify $ \s' -> s' { returnType = ret' }
  return (map snd tvs :-> tv, A.Lambda (map (uncurry C.Annoted) tvs) e')
inferExpression (C.Located _ (C.Mutable expr)) = do
  (t, e') <- local' $ inferExpression expr
  return (Mutable t, A.Mutable e')
inferExpression (C.Located pos (C.Dereference expr)) = do
  tv <- fresh
  (t, e') <- local' $ inferExpression expr
  unify (t :~: Mutable tv, pos)
  return (tv, A.Dereference e')
inferExpression (C.Located pos (C.Match expr cases)) = do
  (t, e') <- local' $ inferExpression expr
  (tys, cases') <- L.unzip <$> local' (CM.forM cases (\(pat, expr') -> do
    (t', pat', vars) <- inferPattern pat
    unify (t :~: t', pos)
    (t'', e'') <- withVariables (M.toList vars) $ inferExpression expr'
    return ((t', t''), (pat', e''))))

  (ret, xs) <- case tys of
    [] -> E.throwError ("Empty match", Nothing, pos)
    ((_, ret):xs) -> return (ret, xs)

  CM.forM_ xs (\(_, t') -> unify (ret :~: t', pos))

  return (ret, A.Match e' cases')
inferExpression (C.Located pos _) = E.throwError ("Invalid expression", Nothing, pos)

inferUpdate :: Infer C.Updated m A.Updated
inferUpdate (C.Located pos (C.VariableUpdate (D.Simple name))) = do
  (env, _) <- ask
  case M.lookup name env of
    Nothing -> E.throwError ("Unbound variable: " ++ name, Nothing, pos)
    Just scheme -> do
      tv <- fresh
      t <- instantiate scheme
      return (tv, A.VariableUpdate name t)
inferUpdate (C.Located pos _) = E.throwError ("Unimplemented", Nothing, pos)

inferPattern :: MonadChecker m => C.Located P.Pattern -> m (Type, A.Pattern, M.Map String Scheme)
inferPattern (C.Located _ (P.VariablePattern (D.Simple name))) = do
  env <- snd <$> ask
  case M.lookup name env of
    Nothing -> do
      tv <- fresh
      return (tv, A.PVariable name tv, M.singleton name (Forall [] tv))
    Just scheme -> do
      t <- instantiate scheme
      return (t, A.PConstructor name [], M.empty)
inferPattern (C.Located _ (P.LiteralPattern lit)) = do
  (t, lit') <- inferLiteral lit
  return (t, A.PLiteral lit', M.empty)
inferPattern (C.Located pos (P.ListPattern ps)) = do
  tv <- fresh
  (ts, ps', envs) <- L.unzip3 <$> mapM inferPattern ps
  CM.unless (length ts == 0) $ unify (TList (last ts) :~: tv, pos)
  return (TList tv, A.PList ps', M.unions envs)
inferPattern (C.Located _ (P.StructurePattern ps)) = do
  (fields, ps', envs) <- L.unzip3 <$> mapM (\(name, p) -> do
    (t, p', env) <- inferPattern p
    return ((name, p'), (name, t), env)) ps
  return (TRec ps', A.PStructure fields, M.unions envs)
inferPattern (C.Located _ (P.WildcardPattern)) = do
  tv <- fresh
  return (tv, A.PWildcard, M.empty)
inferPattern (C.Located pos (P.ConstructorPattern (D.Simple name) pats)) = do
  env <- snd <$> ask
  tv <- fresh
  (t, p, env') <- case M.lookup name env of
    Nothing -> E.throwError ("Unbound constructor: " ++ name, Nothing, pos)
    Just scheme -> do
      t <- instantiate scheme
      return (t, name, M.empty)
  (t', pats', envs) <- L.unzip3 <$> mapM inferPattern pats
  unify (t :~: (t' :-> tv), pos)
  return (tv, A.PConstructor p pats', M.unions envs `M.union` env')
inferPattern (C.Located pos _) = E.throwError ("Unimplemented", Nothing, pos)

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

inferToplevel :: Infer C.Toplevel m [A.Toplevel]
inferToplevel (C.Located pos (C.Function (C.Annoted name ret) generics' args expr)) = do
  generics'' <- M.fromList <$> mapM (\n -> (n,) <$> fresh) generics'
  ret' <- toWithEnv ret generics''
  ST.modify $ \s' -> s' { returnType = ret' }

  argsTypes <- mapM (\(C.Annoted name' t) -> (name',) <$> toWithEnv t generics'') args

  gens <- mapM (\case
    TVar i -> return i
    _ -> E.throwError ("Invalid generic", Nothing, pos)) $ M.elems generics''

  let funTy = map snd argsTypes :-> ret'
  let args' = (name, Forall gens funTy) : map (\(n, t) -> (n, Forall gens t)) argsTypes

  (t, e') <- withGenerics generics'' $ withVariables args' $ inferExpression expr
  unify (t :~: ret', pos)

  csts <- ST.gets constraints
  s <- solve csts
  ST.modify $ \s' -> s' { constraints = [] }

  let scheme = Forall gens $ apply s funTy
  ST.modify $ \s' -> s' { variables = M.insert name scheme (variables s') }
  ST.modify $ \s' -> s' { returnType = Void }
  return (Void, apply s $ [A.Function name (map (uncurry C.Annoted) argsTypes) ret' e'])
inferToplevel (C.Located _ (C.Public tl)) = inferToplevel tl
inferToplevel (C.Located pos (C.Declare name gens args ret)) = do
  generics' <- mapM (const fresh) gens
  let env = M.fromList $ zip gens generics'
  args' <- mapM (mapM (`toWithEnv` env)) args
  ret' <- toWithEnv ret env

  gens' <- mapM (\case
    TVar i -> return i
    _ -> E.throwError ("Invalid generic", Nothing, pos)) generics'

  let ty = case args' of
        Nothing -> ret'
        Just args'' -> args'' :-> ret'

  let scheme = Forall gens' ty
  ST.modify $ \s -> s { variables = M.insert name scheme (variables s) }
  return (Void, [A.Declare generics' (C.Annoted name ty)])
inferToplevel (C.Located pos (C.Enumeration name generics' constructors)) = do
  generics'' <- mapM (const fresh) generics'
  let env = M.fromList $ zip generics' generics''
  let header = if null generics'' then TId name else TApp (TId name) generics''

  constructors' <- mapM (\(C.Annoted name' args) -> do
    args' <- mapM (flip toWithEnv env) args
    return (name', args' :-> header)) constructors

  gens <- mapM (\case
    TVar i -> return i
    _ -> E.throwError ("Invalid generic", Nothing, pos)) generics''

  let schemes = M.fromList $ map (\(n, t) -> (n, Forall gens t)) constructors'
  ST.modify $ \s -> s { types = M.union schemes (types s) }

  -- Building structures types for each constructor
  -- of the following form:
  -- def name = fun(arg₁, arg₂, ..., argₙ) return { type: name, arg₁: arg₁, arg₂: arg₂, ..., argₙ: argₙ }

  structures <- mapM (\(n, t) -> case t of
    args :-> ret -> do
      let args' = zipWith (\t' i -> C.Annoted ("a" ++ show i) t') args [(0 :: Integer)..]
      let variables' = zipWith (\t' i -> A.Variable ("a" ++ show i) t') args [(0 :: Integer)..]
      let fields = zipWith (\_ i ->("a" ++ show i)) args [(0 :: Integer)..]
      return $ A.Function n args' ret (A.Return $ A.Structure $ (zip fields variables') ++ [("type", A.Literal (C.String n)), ("$$enum", A.Literal (C.Bool True))])
    _ -> E.throwError ("Invalid constructor of enumeration", Nothing, pos)) constructors'

  return (Void, structures)
inferToplevel (C.Located pos (C.Type name generics' decl)) = do
  gens <- mapM (const fresh) generics'
  let env = M.fromList $ zip generics' gens

  decl' <- toWithEnv decl env
  gens' <- mapM (\case
    TVar i -> return i
    _ -> E.throwError ("Invalid generic", Nothing, pos)) gens

  ST.modify $ \s -> s { aliases = M.insert name (Forall gens' decl') (aliases s) }

  return (Void, [])
inferToplevel (C.Located pos (C.Declaration (name C.:@ ty) expr)) = do
  tv <- to ty
  (t', e') <- withVariable (name, Forall [] tv) $ inferExpression expr

  unify (tv :~: t', pos)
  let scheme = Forall [] tv
  ST.modify $ \s' -> s' { variables = M.insert name scheme (variables s') }
  return (Void, [A.Declaration name  tv e'])
inferToplevel (C.Located pos _) = E.throwError ("Unimplemented", Nothing, pos)

performInfer :: Monad m => [C.Located C.Toplevel] -> m (Either (String, Maybe String, C.Position) [A.Toplevel])
performInfer toplevels = E.runExceptT $ concat <$> ST.evalStateT (mapM ((snd <$>) . inferToplevel) toplevels) (CheckerState 0 functions mempty [] mempty Void mempty)

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