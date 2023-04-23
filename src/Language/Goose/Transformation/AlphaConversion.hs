{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Goose.Transformation.AlphaConversion where
import qualified Control.Monad.State as ST
import Language.Goose.Transformation.ANF.AST
import Language.Goose.Transformation.Closure.Conversion (substExpr, v, subst)

type MonadAlpha m = (ST.MonadState Int m)

alphaName :: MonadAlpha m => String -> m String
alphaName name = do
  i <- ST.get
  ST.put $ i + 1
  return $ name ++ "$" ++ show i

substManyE :: ANFExpression -> [(String, ANFExpression)] -> ANFExpression
substManyE = foldl (\e (name, expr) -> substExpr e name expr)

substManyS :: ANFStatement -> [(String, ANFExpression)] -> ANFStatement
substManyS = foldl (\e (name, expr) -> subst e name expr)

alphaToplevel :: MonadAlpha m => [ANFDefinition] -> m [ANFDefinition]
alphaToplevel (DFunction name args body : defs) = do
  args' <- mapM alphaName args
  body' <- map (\e -> substManyS e (zipWith (\n e' -> (n, EVariable e' v)) args args')) <$> alphaStatement body
  defs' <- alphaToplevel defs
  return $ DFunction name args' body' : defs'
alphaToplevel (DDeclaration name expr : defs) = do
  defs' <- alphaToplevel defs
  return $ DDeclaration name expr : defs'
alphaToplevel (x:xs) = (x:) <$> alphaToplevel xs
alphaToplevel [] = return []

alphaStatement :: MonadAlpha m => [ANFStatement] -> m [ANFStatement]
alphaStatement (SLet name expr : stmts) = do
  name' <- alphaName name
  let expr' = substExpr expr name (EVariable name' v)
  stmts' <- alphaStatement stmts
  return $ SLet name' expr' : map (\e -> subst e name (EVariable name' v)) stmts'
alphaStatement (x:xs) = (x:) <$> alphaStatement xs
alphaStatement [] = return []

runAlphaConversion :: [ANFDefinition] -> [ANFDefinition]
runAlphaConversion defs = ST.evalState (alphaToplevel defs) 0