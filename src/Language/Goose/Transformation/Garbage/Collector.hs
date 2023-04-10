{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Goose.Transformation.Garbage.Collector where
import Language.Goose.Transformation.Closure.Free
import Language.Goose.Transformation.Closure.Conversion (v)
import Language.Goose.Transformation.ANF.AST
import Language.Goose.CST.Annoted
import qualified Control.Monad.State as ST
import qualified Data.Set as S
import Data.Maybe
type MonadGarbage m = ST.MonadState (S.Set String) m

addExcluded :: MonadGarbage m => String -> m ()
addExcluded name = ST.modify (S.insert name)

removeExcluded :: MonadGarbage m => String -> m ()
removeExcluded name = ST.modify (S.filter (/= name))

local :: MonadGarbage m => m a -> m a
local m = do
  old <- ST.get
  ST.put mempty
  res <- m
  ST.put old
  return res

collect :: MonadGarbage m => [ANFDefinition] -> m [ANFDefinition]
collect = mapM collectDefinition

collectDefinition :: MonadGarbage m => ANFDefinition -> m ANFDefinition
collectDefinition (DDeclaration name expr) = addExcluded name *> (DDeclaration name <$> collectExpression expr)
collectDefinition (DFunction name args body) = do
  addExcluded name
  mapM_ addExcluded args
  body' <- local $ collectSequence [] body
  mapM_ removeExcluded args
  return $ DFunction name args body'
collectDefinition (DDeclare (Annoted name ty)) = addExcluded name *> return (DDeclare (Annoted name ty))
collectDefinition (DExtern (Annoted name ty)) = addExcluded name *> return (DExtern (Annoted name ty))

createFree :: S.Set String -> [ANFStatement]
createFree = map (\n -> SExpression (EApplication (EVariable "free" v) [EVariable n v])) . S.toList

collectStatement :: MonadGarbage m => ANFStatement -> m ANFStatement
collectStatement (SExpression e) = SExpression <$> collectExpression e
collectStatement (SLet name e) = do
  addExcluded name
  let e' = SLet name <$> collectExpression e
  removeExcluded name
  e'
collectStatement (SIf cond t f) = do
  cond' <- collectExpression cond
  t' <- local $ collectSequence [] t
  f' <- local $ collectSequence [] f
  return $ SIf cond' t' f'
collectStatement (SWhile cond body) = do
  cond' <- collectExpression cond
  body' <- local $ collectSequence [] body
  return $ SWhile cond' body'
collectStatement (SUpdate n e) = SUpdate n <$> collectExpression e
collectStatement (SReturn e) = SReturn <$> collectExpression e
collectStatement (SFor name list body) = do
  list' <- collectExpression list
  body' <- local $ collectSequence [] body
  return $ SFor name list' body'
collectStatement (SBreak) = return SBreak
collectStatement (SContinue) = return SContinue
collectStatement (SMatch e cases) = do
  e' <- collectExpression e
  cases' <- mapM (\(p, b) -> (,) p <$> local (collectSequence [] b)) cases
  return $ SMatch e' cases'
collectStatement (SBlock _) = error "Should not encounter blocks during garbage collection"

collectSequence :: MonadGarbage m => [String] -> [ANFStatement] -> m [ANFStatement]
collectSequence garbaged (SLet n expr:xs) = do
  s <- ST.get
  addExcluded n
  expr' <- collectExpression expr
  xs' <- collectSequence (if n `S.member` s then garbaged else (n:garbaged)) xs
  removeExcluded n
  return $ SLet n expr':xs'
collectSequence garbaged z@(x:xs) = do
  x' <- collectStatement x
  let free1 = free z
  let decls = map (\n -> if n `S.member` free1 
          then Nothing
          else Just (SExpression $ EApplication (EVariable "free" v) [EVariable n v])) garbaged
  let garbaged' = filter (`S.member` free1) garbaged
  xs' <- collectSequence garbaged' xs
  return $ (catMaybes decls) ++ [x'] ++ xs'
collectSequence garbaged [] = return $ map (SExpression . EApplication (EVariable "free" v) . (:[]) . (`EVariable` v)) garbaged


collectExpression :: MonadGarbage m => ANFExpression -> m ANFExpression
collectExpression (EApplication f args) = EApplication <$> collectExpression f <*> mapM collectExpression args
collectExpression (EIf cond t f) = EIf <$> collectExpression cond <*> collectExpression t <*> (case f of
  Nothing -> return Nothing
  Just e -> Just <$> collectExpression e)
collectExpression (EVariable name t) = return $ EVariable name t
collectExpression (ELiteral l) = return $ ELiteral l
collectExpression (ELambda {}) = error "Should not encounter lambdas during garbage collection"
collectExpression (EUpdate e v) = EUpdate e <$> collectExpression v
collectExpression (EBinary op e1 e2) = EBinary op <$> collectExpression e1 <*> collectExpression e2
collectExpression (EUnary op e) = EUnary op <$> collectExpression e
collectExpression (EList es) = EList <$> mapM collectExpression es
collectExpression (EListAccess e1 e2) = EListAccess <$> collectExpression e1 <*> collectExpression e2
collectExpression (EStructure fields) = EStructure <$> mapM (\(n, e) -> (,) n <$> collectExpression e) fields
collectExpression (EStructAccess e1 e2) = EStructAccess <$> collectExpression e1 <*> return e2

runCollector :: [ANFDefinition] -> [ANFDefinition]
runCollector xs = ST.evalState (collect xs) $ S.fromList ["makeLambda", "free"]