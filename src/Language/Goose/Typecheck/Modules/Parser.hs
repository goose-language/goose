{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Language.Goose.Typecheck.Modules.Parser where
import qualified Language.Goose.Typecheck.Definition.Type as T
import qualified Language.Goose.CST.Modules.Declaration as D
import qualified Language.Goose.Typecheck.Definition.Monad as T
import qualified Data.Map as M

class Parser a where
  to :: T.MonadChecker m => a -> m T.Type
  to = (`toWithEnv` M.empty)
  toWithEnv :: T.MonadChecker m => a -> M.Map String T.Type -> m T.Type
  from :: T.Type -> a

instance Parser D.Declaration where
  toWithEnv = go
    where go :: T.MonadChecker m => D.Declaration -> M.Map String T.Type -> m T.Type
          go (D.Constructor v xs) env = do
            t <- go v env
            ts <- mapM (flip go env) xs

            return (T.TApp t ts)
          go (D.Generic v) env = case M.lookup v env of
            Just t -> return t
            Nothing -> T.fresh
          go (D.ID (D.Simple v)) _ = return (T.TId v)
          go D.Int _ = return (T.Int)
          go D.Bool _ = return (T.Bool)
          go D.Float _ = return (T.Float)
          go D.Char _ = return (T.Char)
          go D.Unit _ = return (T.Void)
          go (D.List v) env = T.TApp (T.TId "List") . (:[]) <$> go v env
          go _ _ = undefined
  from = undefined