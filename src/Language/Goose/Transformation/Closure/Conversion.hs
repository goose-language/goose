{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
module Language.Goose.Transformation.Closure.Conversion where
import Language.Goose.Transformation.ANF.AST
import Language.Goose.CST.Annoted
import Language.Goose.Transformation.Closure.Free (Free(free))
import qualified Data.Set as S
import qualified Data.Map as M
import Language.Goose.Typecheck.Checker
import Control.Monad.State
import Data.List (nub)
import qualified Language.Goose.CST.Literal as C

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
  expr' <- concat <$> mapM convertStatement body
  addExcluded name
  return $ DFunction name args expr'
convertToplevel (DDeclaration name expr) = do
  expr' <- convertExpression expr
  return $ DDeclaration name expr'
convertToplevel (DFunction name args body) = do
  addExcluded name
  expr' <- concat <$> mapM convertStatement body
  return $ DFunction name args expr'
convertToplevel (DDeclare (Annoted name ty)) = do
  addExcluded name
  return $ DDeclare (Annoted name ty)
convertToplevel x = return x

closureConvert :: MonadClosure m => ANFExpression -> String -> m ANFExpression
closureConvert z@(ELambda args expr) name = do
  excluded <- gets snd
  let freed = if not (null name) then free z `S.union` S.singleton name else free z 
  let env = S.filter (`notElem` excluded) freed
  let declarations = nub $ map (\(n) -> SLet n (EStructAccess (EVariable "env") n)) $ S.toList env

  expr' <- mapM convertStatement expr
  let body = case expr' of
                stmts -> declarations ++ concat stmts

  let envStruct = map (\n -> if n == name then (n, EStructure [("$$func", ELiteral C.Unit), ("env", ELiteral C.Unit)]) else (n, EVariable n)) $ S.toList env

  return $ EStructure [("$$func", EApplication (EVariable "makeLambda") [ELambda ("env": args) body]), ("env", EStructure $ envStruct)]
closureConvert _ _ = undefined

convertStatement :: MonadClosure m => ANFStatement -> m [ANFStatement]
convertStatement (SExpression e) = (:[]) . SExpression <$> convertExpression e
convertStatement (SLet name z@ELambda {}) = do
  expr' <- closureConvert z name
  if name `S.member` free z
    then return $ [SLet name expr', SUpdate (UStructAccess (UStructAccess (UVariable name) "env") name) (EVariable name)] 
    else return $ [SLet name expr']
convertStatement (SLet name e) = do
  expr' <- convertExpression e
  if name `S.member` free e 
    then return $ [SLet name expr', SUpdate (UStructAccess (UStructAccess (UVariable name) "env") name) (EVariable name)] 
    else return $ [SLet name expr']
convertStatement (SIf cond t f) = do
  cond' <- convertExpression cond
  t' <- concat <$> mapM convertStatement t
  f' <- concat <$> mapM convertStatement f
  return $ [SIf cond' t' f']
convertStatement (SWhile cond body) = do
  cond' <- convertExpression cond
  body' <- concat <$> mapM convertStatement body
  return $ [SWhile cond' body']
convertStatement (SUpdate n e) = do
  e' <- convertExpression e
  return $ [SUpdate n e']
convertStatement (SReturn e) = do
  e' <- convertExpression e
  return $ [SReturn e']
convertStatement (SFor name list body) = do
  list' <- convertExpression list
  body' <- concat <$> mapM convertStatement body
  return $ [SFor name list' body']
convertStatement (SBlock xs) = concat <$> mapM convertStatement xs
convertStatement (SBreak) = return [SBreak]
convertStatement (SContinue) = return [SContinue]

convertExpression :: MonadClosure m => ANFExpression -> m ANFExpression
convertExpression (EApplication (EVariable x) args) = do
  excluded <- isExcluded x
  if not excluded
    then do
      args' <- mapM convertExpression args
      let call = EApplication (EStructAccess (EStructAccess (EVariable x) "$$func") "$$fun") (EStructAccess (EVariable x) "env" : args')
      return call
    else EApplication (EVariable x) <$> mapM convertExpression args
convertExpression (EApplication f args) = do
  f' <- convertExpression f
  args' <- mapM convertExpression args
  return $ EApplication (EStructAccess f' "$$fun") args'
convertExpression (EIf cond t f) = do
  cond' <- convertExpression cond
  t' <- convertExpression t
  f' <- case f of
    Just f' -> Just <$> convertExpression f'
    Nothing -> return Nothing
  return $ EIf cond' t' f'
convertExpression (ELiteral x) = return $ ELiteral x
convertExpression (EList xs) = EList <$> mapM convertExpression xs
convertExpression z@(ELambda {}) = closureConvert z ""
convertExpression (EListAccess arr idx) = EListAccess <$> convertExpression arr <*> convertExpression idx
convertExpression (EStructAccess str name) = EStructAccess <$> convertExpression str <*> pure name
convertExpression (EStructure xs) = EStructure <$> mapM (mapM convertExpression) xs
convertExpression (EVariable x) = do
  b <- isExcluded x
  if b
    then return $ EVariable x
    else return $ EVariable x
convertExpression (EBinary op e1 e2) = EBinary op <$> convertExpression e1 <*> convertExpression e2
convertExpression (EUnary op e) = EUnary op <$> convertExpression e
convertExpression (EUpdate updated e) = EUpdate updated <$> convertExpression e

runClosureConversion :: [ANFDefinition] -> [ANFDefinition]
runClosureConversion xs = do
  let (res, _, _) = foldl (\(acc, i, excl) x -> do
                          let (str, (i', excluded)) = runState (convertToplevel x) (i, excl)
                          (acc ++ ([str]), i', excl `S.union` excluded)) ([], 0, S.fromList $ M.keys functions) xs
  res
