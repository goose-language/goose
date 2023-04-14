{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.Goose.Transformation.EtaExpansion where

import Language.Goose.Typecheck.Definition.AST
import Language.Goose.Typecheck.Definition.Type hiding (pattern Mutable)
import Language.Goose.Typecheck.Definition.Free

import qualified Language.Goose.CST.Annoted as C
import qualified Control.Monad.State as ST
import qualified Data.Set as S

fresh :: MonadEta m => m Int
fresh = do
  i <- ST.gets fst
  ST.modify $ \s -> (i + 1, snd s)
  return i

freshName :: MonadEta m => m String
freshName = do
  i <- fresh
  return $ "$_$a" ++ show i

local :: MonadEta m => m a -> m a
local m = do
  s <- ST.get
  r <- m
  cnt <- ST.gets fst
  ST.put (cnt, snd s)
  return r

addEtaExpanded :: MonadEta m => String -> m ()
addEtaExpanded name = do
  ST.modify $ \s -> (fst s, name : snd s)

type MonadEta m = ST.MonadState (Int, [String]) m

etaExpandExpr :: MonadEta m => Expression -> m Expression
etaExpandExpr (Let (name C.:@ t) expr body) = 
  case t of
    arg :-> _ -> case expr of
      Lambda {} -> Let (name C.:@ t) <$> local (etaExpandExpr expr) <*> (local $ etaExpandExpr body)
      _ -> do
        if (name `S.member` free expr) then do
          n <- mapM (const freshName) arg
          let args = zipWith (C.:@) n arg
          let callArgs = zipWith Variable n arg
          addEtaExpanded name
          body' <- local $ etaExpandExpr body
          expr' <- local $ etaExpandExpr expr
          return $ Let (name C.:@ t) (Lambda args (Application expr' callArgs)) body'
        else do
          expr' <- local $ etaExpandExpr expr
          body' <- local $ etaExpandExpr body
          return $ Let (name C.:@ t) expr' body'
    _ -> Let (name C.:@ t) <$> local (etaExpandExpr expr) <*> (local $ etaExpandExpr body)
etaExpandExpr (Lambda args expr) = Lambda args <$> (local $ etaExpandExpr expr)
etaExpandExpr (Application expr args) = Application <$> (local $ etaExpandExpr expr) <*> (local $ mapM etaExpandExpr args)
etaExpandExpr (Variable name t) = do
  ST.gets snd >>= \e -> 
    if name `elem` e then
      return $ Variable name t
    else
      return $ Variable name t
etaExpandExpr (Literal lit) = return $ Literal lit
etaExpandExpr (If cond t f) = If <$> (local $ etaExpandExpr cond) <*> (local $ etaExpandExpr t) <*> case f of
  Just f' -> Just <$> (local $ etaExpandExpr f')
  Nothing -> return Nothing
etaExpandExpr (Match expr cases) = Match <$> (local $ etaExpandExpr expr) <*> (local $ mapM (mapM etaExpandExpr) cases)
etaExpandExpr (Sequence exprs) = Sequence <$> (local $ mapM etaExpandExpr exprs)
etaExpandExpr (List exprs) = List <$> (mapM etaExpandExpr exprs)
etaExpandExpr (ListAccess e i) = ListAccess <$> (etaExpandExpr e) <*> (etaExpandExpr i)
etaExpandExpr (While cond body) = While <$> (local $ etaExpandExpr cond) <*> (local $ mapM etaExpandExpr body)
etaExpandExpr (For name list body) = For name <$> (local $ etaExpandExpr list) <*> (local $ mapM etaExpandExpr body)
etaExpandExpr (Return e) = Return <$> (local $ etaExpandExpr e)
etaExpandExpr (Binary op e1 e2) = Binary op <$> (local $ etaExpandExpr e1) <*> (local $ etaExpandExpr e2)
etaExpandExpr (Structure fields) = Structure <$> (local $ mapM (mapM etaExpandExpr) fields)
etaExpandExpr (StructureAccess e name) = StructureAccess <$> (local $ etaExpandExpr e) <*> return name
etaExpandExpr (Update u e) = Update u <$> (local $ etaExpandExpr e)
etaExpandExpr (Mutable e) = Mutable <$> (local $ etaExpandExpr e)
etaExpandExpr (Dereference e) = Dereference <$> (local $ etaExpandExpr e)
etaExpandExpr _ = error "Not implemented"

etaExpandTL :: MonadEta m => Toplevel -> m Toplevel
etaExpandTL (Function name args ret expr) = Function name args ret <$> (etaExpandExpr expr)
etaExpandTL (Declaration name ret expr) = Declaration name ret <$> (etaExpandExpr expr)
etaExpandTL (Declare name t) = return $ Declare name t
etaExpandTL (Extern (name C.:@ t)) = return $ Extern (name C.:@ t)
etaExpandTL _ = error "Not implemented"

runEtaExpansion :: [Toplevel] -> [Toplevel]
runEtaExpansion toplevels = ST.evalState (mapM etaExpandTL toplevels) (0, [])