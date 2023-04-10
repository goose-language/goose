{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Goose.Transformation.Closure.Hoisting where
import Language.Goose.Transformation.ANF.AST
import Language.Goose.CST.Annoted
import Control.Monad.RWS
import Language.Goose.Typecheck.Definition.Type

type MonadHoist m = (MonadRWS () [ANFDefinition] Int m)
  
freshName :: MonadHoist m => m String
freshName = do
  i <- get
  put (i+1)
  return $ "$lambda" ++ show i

convertToplevel :: MonadHoist m => ANFDefinition -> m (ANFDefinition)
convertToplevel (DDeclaration name (ELambda args body)) = do
  expr' <- mapM convertStatement body
  return $ DFunction name args expr'
convertToplevel (DDeclaration name expr) = do
  expr' <- convertExpression expr
  return $ DDeclaration name expr'
convertToplevel (DFunction name args body) = do
  expr' <- mapM convertStatement body
  return $ DFunction name args expr'
convertToplevel (DDeclare (Annoted name ty)) = do
  return $ DDeclare (Annoted name ty)
convertToplevel x = return x

convertStatement :: MonadHoist m => ANFStatement -> m ANFStatement
convertStatement (SExpression e) = SExpression <$> convertExpression e
convertStatement (SLet name e) = do
  expr' <- convertExpression e
  return $ SLet name expr'
convertStatement (SIf cond t f) = do
  cond' <- convertExpression cond
  t' <- mapM convertStatement t
  f' <- mapM convertStatement f
  return $ SIf cond' t' f'
convertStatement (SWhile cond body) = do
  cond' <- convertExpression cond
  body' <- mapM convertStatement body
  return $ SWhile cond' body'
convertStatement (SUpdate n e) = do
  e' <- convertExpression e
  return $ SUpdate n e'
convertStatement (SReturn e) = do
  e' <- convertExpression e
  return $ SReturn e'
convertStatement (SFor name list body) = do
  list' <- convertExpression list
  body' <- mapM convertStatement body
  return $ SFor name list' body'
convertStatement (SBreak) = return SBreak
convertStatement (SContinue) = return SContinue
convertStatement (SBlock _) = error "Should not encounter blocks during hoisting"
convertStatement (SMatch expr cases) = do
  expr' <- convertExpression expr
  cases' <- mapM (\(p, b) -> (,) p <$> mapM convertStatement b) cases
  return $ SMatch expr' cases'

convertExpression :: MonadHoist m => ANFExpression -> m ANFExpression
convertExpression (EApplication f args) = EApplication <$> convertExpression f <*> mapM convertExpression args
convertExpression (EIf cond t f) = do
  cond' <- convertExpression cond
  t' <- convertExpression t
  f' <- case f of
    Just f' -> Just <$> convertExpression f'
    Nothing -> return Nothing
  return $ EIf cond' t' f'
convertExpression (ELiteral x) = return $ ELiteral x
convertExpression (EList xs) = EList <$> mapM convertExpression xs
convertExpression (ELambda args body) = do
  name <- freshName
  body' <- mapM convertStatement body
  tell [DFunction name args body']
  return $ EVariable name (TId "Value")
convertExpression (EListAccess arr idx) = EListAccess <$> convertExpression arr <*> convertExpression idx
convertExpression (EStructAccess str name) = EStructAccess <$> convertExpression str <*> pure name
convertExpression (EStructure xs) = EStructure <$> mapM (mapM convertExpression) xs
convertExpression (EVariable x t) = return $ EVariable x t
convertExpression (EBinary op e1 e2) = EBinary op <$> convertExpression e1 <*> convertExpression e2
convertExpression (EUnary op e) = EUnary op <$> convertExpression e
convertExpression (EUpdate updated e) = EUpdate updated <$> convertExpression e

runHoisting :: [ANFDefinition] -> [ANFDefinition]
runHoisting xs = do
  fst $ foldl (\(acc, i) x -> do
    let (str, i', s) = runRWS (convertToplevel x) () i
    (acc ++ (s ++ [str]), i')) ([], 0) xs
