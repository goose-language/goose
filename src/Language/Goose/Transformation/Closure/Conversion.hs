{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
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

type MonadClosure m = (MonadState (Int, S.Set String) m)

fresh :: MonadClosure m => m Int
fresh = do
  (i, e) <- get
  put (i + 1, e)
  return i

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

convertToplevel :: MonadClosure m => ANFDefinition -> m (ANFDefinition)
convertToplevel (DDeclaration name (ELambda args body)) = do
  addExcluded name
  expr' <- concat <$> mapM convertStatement body
  return $ DFunction name (args ++ ["env"]) expr'
convertToplevel (DDeclaration name expr) = do
  (expr', _) <- convertExpression expr
  return $ DDeclaration name expr'
convertToplevel (DFunction name args body) = do
  addExcluded name
  expr' <- concat <$> mapM convertStatement body
  return $ DFunction name (if name == "main" then [] else (args ++ ["env"])) expr'
convertToplevel (DDeclare (Annoted name ty)) = do
  addExcluded name
  return $ DDeclare (Annoted name ty)
convertToplevel x = return x

v :: Type
v = TId "Value"

closureConvert :: MonadClosure m => ANFExpression -> String -> m ANFExpression
closureConvert z@(ELambda args expr) name = do
  (_, excluded) <- get
  let freed = if not (null name) then free z `S.union` S.singleton name else free z 
  let env = S.filter (`notElem` excluded) freed
  let declarations = nub $ map (\(n) -> SLet n (EStructAccess (EVariable "env" v) n)) $ S.toList env

  expr' <- mapM convertStatement expr

  let body = case expr' of
                stmts -> declarations ++ concat stmts

  let envStruct = map (\n -> if n == name then (n, EStructure [("$$func", ELiteral C.Unit), ("env", ELiteral C.Unit)]) else (n, EVariable n v)) $ S.toList env

  return $ EStructure [("$$func", EApplication (EVariable "makeLambda" v) [ELambda ("env": args) body]), ("env", EStructure $ envStruct)]
closureConvert _ _ = undefined

convertStatement :: MonadClosure m => ANFStatement -> m [ANFStatement]
convertStatement (SExpression e) = do
  (e', stmts) <- convertExpression e
  return $ stmts ++ [SExpression e']
convertStatement (SLet name z@ELambda {}) = do
  expr' <- closureConvert z name
  if name `S.member` free z
    then return $ [SLet name expr', SUpdate (UStructAccess (UStructAccess (UVariable name) "env") name) (EVariable name v)] 
    else return $ [SLet name expr']
convertStatement (SLet name e) = do
  (expr', stmts) <- convertExpression e
  if name `S.member` free e 
    then return $ stmts ++ [SLet name expr', SUpdate (UStructAccess (UStructAccess (UVariable name) "env") name) (EVariable name v)] 
    else return $ stmts ++ [SLet name expr']
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
convertStatement (SBlock xs) = concat <$> mapM convertStatement xs
convertStatement (SBreak) = return [SBreak]
convertStatement (SContinue) = return [SContinue]
convertStatement (SMatch e cases) = do
  (e', stmts) <- convertExpression e
  cases' <- mapM (\(p, b) -> (,) p <$> concat <$> mapM convertStatement b) cases
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
      return (EApplication (EVariable x v) (args' ++ [EStructure [("$$func", ELiteral C.Unit), ("env", ELiteral C.Unit)]]), concat stmts)
convertExpression (EApplication f args) = do
  (f', stmts1) <- convertExpression f
  (args', stmts2) <- unzip <$> mapM convertExpression args
  n <- freshName
  let decl = SLet n f'
  let var = EVariable n v
  return $ (EApplication (EStructAccess (EStructAccess var "$$func") "$$fun") (EStructAccess var "env" : args'), stmts1 ++ concat stmts2 ++ [decl])
convertExpression (EIf cond t f) = do
  (cond', stmts1) <- convertExpression cond
  (t', stmts2) <- convertExpression t
  (f', stmts3) <- case f of
    Just f' -> convertExpression f'
    Nothing -> return (ELiteral C.Unit, [])
  return $ (EIf cond' t' (Just f'), stmts1 ++ stmts2 ++ stmts3)
convertExpression (ELiteral x) = return $ (ELiteral x, [])
convertExpression (EList xs) = B.bimap EList concat <$> (unzip <$> mapM convertExpression xs)
convertExpression z@(ELambda {}) = (,[]) <$> closureConvert z ""
convertExpression (EListAccess arr idx) = do
  (arr', stmts1) <- convertExpression arr
  (idx', stmts2) <- convertExpression idx
  return $ (EListAccess arr' idx', stmts1 ++ stmts2)
convertExpression (EStructAccess str name) = do
  (str', stmts) <- convertExpression str
  return $ (EStructAccess str' name, stmts)
convertExpression (EStructure xs) = do
  (xs', stmts) <- B.second unzip . unzip <$> mapM (\(n, e) -> (n,) <$> convertExpression e) xs
  return $ (EStructure (zip xs' $ fst stmts), concat $ snd stmts)
convertExpression (EVariable x t) = do
  case t of
    _ :-> _ -> do
      excluded <- isExcluded x
      if not excluded
        then return $ (EVariable x v, [])
        else return $ (EStructure [("$$func", EApplication (EVariable "makeLambda" v) [EVariable x v]), ("env", ELiteral C.Unit)], [])
    _ -> return $ (EVariable x v, [])
convertExpression (EBinary op e1 e2) = do
  (e1', stmts1) <- convertExpression e1
  (e2', stmts2) <- convertExpression e2
  return $ (EBinary op e1' e2', stmts1 ++ stmts2)
convertExpression (EUnary op e) = do
  (e', stmts) <- convertExpression e
  return $ (EUnary op e', stmts)
convertExpression (EUpdate updated e) = do
  (e', stmts) <- convertExpression e
  return $ (EUpdate updated e', stmts)

runClosureConversion :: [ANFDefinition] -> [ANFDefinition]
runClosureConversion xs = do
  let (res, _, _) = foldl (\(acc, i, excl) x -> do
                          let (str, (i', excluded)) = runState (convertToplevel x) (i, excl)
                          (acc ++ ([str]), i', excl `S.union` excluded)) ([], 0, S.fromList $ M.keys functions) xs
  res
