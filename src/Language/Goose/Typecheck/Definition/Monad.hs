{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Goose.Typecheck.Definition.Monad where
import Data.Map
import Control.Monad.RWS hiding (ask, local)
import Control.Monad.Except
import Language.Goose.Typecheck.Definition.Type
import Language.Goose.CST.Located
import Language.Goose.Typecheck.Modules.Substitution
import Language.Goose.Typecheck.Modules.Apply

data CheckerState = CheckerState {
  counter :: Int,
  variables :: Map String Scheme,
  types :: Map String Scheme,
  constraints :: [Constraint],
  generics :: Map String Type,
  returnType :: Type,
  aliases :: Map String Scheme
}

union' :: Environment -> Environment -> Environment
union' (vars1, types1) (vars2, types2) = (vars1 `union` vars2, types1 `union` types2)

type MonadChecker m = (MonadState CheckerState m, MonadError (String, Maybe String, Position) m)

fresh :: MonadChecker m => m Type
fresh = gets counter >>= \n -> modify (\s -> s { counter = n + 1 }) >> return (TVar n)

instantiate :: MonadChecker m => Scheme -> m Type
instantiate (Forall vars t) = do
  vars' <- mapM (const fresh) vars
  let s = fromList $ zip vars vars'
    in return $ apply s t

ask :: MonadChecker m => m Environment
ask = (,) <$> gets variables <*> gets types

local :: MonadChecker m => (Environment -> Environment) -> m a -> m a
local f m = do
  (vars, types') <- ask
  let (vars', types'') = f (vars, types')
  modify (\s -> s { variables = vars', types = types'' })
  a <- m
  cnt' <- gets counter
  csts <- gets constraints
  modify (\s -> s { variables = vars, types = types', counter = cnt', constraints = csts })
  return a
      
local' :: MonadChecker m => m a -> m a
local' = local id