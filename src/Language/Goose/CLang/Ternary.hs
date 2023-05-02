{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Goose.CLang.Ternary where
import Language.Goose.CLang.Definition.IR
import Control.Monad.State
import qualified Language.Goose.CST.Literal as C

type MonadTernary m = MonadState Int m

freshName :: MonadTernary m => m String
freshName = do
  i <- get
  put $ i + 1
  return $ "x" ++ show i

transformE :: MonadTernary m => IRExpression -> m (IRExpression, [IRStatement])
transformE (IRVariable x) = return (IRVariable x, [])
transformE (IRApplication f xs) = do
  (f', s1) <- transformE f
  (xs', s2) <- unzip <$> mapM transformE xs
  return (IRApplication f' xs', s1 ++ concat s2)
transformE (IRLiteral l) = return (IRLiteral l, [])
transformE (IRList xs) = do
  (xs', s) <- unzip <$> mapM transformE xs
  return (IRList xs', concat s)
transformE (IRListAccess x i) = do
  (x', s1) <- transformE x
  (i', s2) <- transformE i
  return (IRListAccess x' i', s1 ++ s2)
transformE (IRDictAccess x i) = do
  (x', s1) <- transformE x
  return (IRDictAccess x' i, s1)
transformE (IRIn x i) = do
  (x', s1) <- transformE x
  return (IRIn x' i, s1)
transformE (IRBinary op x y) = do
  (x', s1) <- transformE x
  (y', s2) <- transformE y
  return (IRBinary op x' y', s1 ++ s2)
transformE (IRUnary op x) = do
  (x', s) <- transformE x
  return (IRUnary op x', s)
transformE (IRTernary cond then' else') = do
  (cond', s1) <- transformE cond
  (then'', s2) <- transformE then'
  (else'', s3) <- transformE else'
  n <- freshName
  return (IRVariable n, s1 ++ [IRDeclarationStatement n (IRLiteral C.Unit), IRIfElse cond' (s2 ++ [IRUpdate (IRVariable n) then'']) (s3 ++ [IRUpdate (IRVariable n) else''])]) 
transformE (IREUpdate x y) = do
  (x', s1) <- transformE x
  (y', s2) <- transformE y
  return (IREUpdate x' y', s1 ++ s2)
transformE (IRDict xs) = do
  (xs', s) <- unzip <$> mapM (\(k, v) -> do
    (v', s) <- transformE v
    return ((k, v'), s)) xs
  return (IRDict xs', concat s)

transformS :: MonadTernary m => IRStatement -> m [IRStatement]
transformS (IRReturn x) = do
  (x', s) <- transformE x
  return $ s ++ [IRReturn x']
transformS (IRIfElse cond then' else') = do
  (cond', s1) <- transformE cond
  then'' <- concat <$> mapM transformS then'
  else'' <- concat <$> mapM transformS else'
  return $ s1 ++ [IRIfElse cond' then'' else'']
transformS (IRIf cond then') = do
  (cond', s1) <- transformE cond
  then'' <- concat <$> mapM transformS then'
  return $ s1 ++ [IRIf cond' then'']
transformS (IRWhile cond body) = do
  (cond', s1) <- transformE cond
  body' <- concat <$> mapM transformS body
  return $ s1 ++ [IRWhile cond' body']
transformS (IRFor x xs body) = do
  (xs', s1) <- transformE xs
  body' <- concat <$> mapM transformS body
  return $ s1 ++ [IRFor x xs' body']
transformS (IRExpression x) = do
  (x', s) <- transformE x
  return $ s ++ [IRExpression x']
transformS (IRBlock xs) = do
  xs' <- concat <$> mapM transformS xs
  return [IRBlock xs']
transformS (IRDeclarationStatement x y) = do
  (y', s) <- transformE y
  return $ s ++ [IRDeclarationStatement x y']
transformS (IRUpdate x y) = do
  (y', s) <- transformE y
  return $ s ++ [IRUpdate x y']
transformS IRBreak = return [IRBreak]
transformS IRContinue = return [IRContinue]

transformT :: MonadTernary m => IRToplevel -> m IRToplevel
transformT (IRFunction name args body) = do
  body' <- concat <$> mapM transformS body
  return $ IRFunction name args body'
transformT (IRDeclaration name expr) = do
  (expr', _) <- transformE expr
  return $ IRDeclaration name expr'
transformT x = return x

runTernaryTransform :: [IRToplevel] -> [IRToplevel]
runTernaryTransform xs = evalState (mapM transformT xs) 0