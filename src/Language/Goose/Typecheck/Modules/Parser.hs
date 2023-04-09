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

instance Parser a => Parser (Maybe a) where
  toWithEnv x env = case x of
    Just x' -> toWithEnv x' env
    Nothing -> T.fresh
  from = undefined

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
          go (D.Function args ret) env = do
            ts <- mapM (flip go env) args
            t <- go ret env
            return $ ts T.:-> t
          go (D.List v) env = T.TApp (T.TId "List") . (:[]) <$> go v env
          go (D.Structure xs) env = do
            ts <- mapM (flip go env . snd) xs
            return (T.TRec (zip (map fst xs) ts))
          go _ _ = undefined
  from = undefined