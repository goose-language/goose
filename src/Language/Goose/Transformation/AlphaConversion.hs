{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Goose.Transformation.AlphaConversion where
import qualified Control.Monad.State as ST
import Language.Goose.Transformation.ANF.AST
import Language.Goose.Transformation.Closure.Conversion (substExpr, v, subst)
import Language.Goose.Typecheck.Definition.AST (Pattern(PVariable, PConstructor, PStructure, PList, PLiteral, PWildcard))
import qualified Data.Map as M
import Data.Bifunctor (Bifunctor(second))

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
alphaStatement (SMatch expr cases : stmts) = do
  cases' <- mapM (\(pat, body) -> do
    (pat', m) <- alphaPattern pat
    body' <- mapM substManyS <$> alphaStatement body <*> pure (M.toList . M.map (`EVariable` v) $ m)
    return (pat', body')) cases
  stmts' <- alphaStatement stmts
  return $ SMatch expr cases' : stmts'
alphaStatement (x:xs) = (x:) <$> alphaStatement xs
alphaStatement [] = return []

alphaPattern :: MonadAlpha m => Pattern -> m (Pattern, M.Map String String)
alphaPattern (PVariable name t) = do
  name' <- alphaName name
  return (PVariable name' t, M.singleton name name')
alphaPattern (PConstructor name xs) = do
  xs' <- mapM alphaPattern xs
  return (PConstructor name (map fst xs'), M.unions $ map snd xs')
alphaPattern (PStructure xs) = do
  xs' <- mapM (\(name, pat) -> do
    pat' <- alphaPattern pat
    return (name, pat')) xs
  return (PStructure (map (second fst) xs'), M.unions $ map (snd . snd) xs')
alphaPattern (PList xs) = do
  xs' <- mapM alphaPattern xs
  return (PList (map fst xs'), M.unions $ map snd xs')
alphaPattern (PLiteral l) = return (PLiteral l, M.empty)
alphaPattern PWildcard = return (PWildcard, M.empty)

runAlphaConversion :: [ANFDefinition] -> [ANFDefinition]
runAlphaConversion defs = ST.evalState (alphaToplevel defs) 0