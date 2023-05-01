{-# LANGUAGE FlexibleContexts #-}


module Language.Goose.Typecheck.Modules.Parser where
import qualified Language.Goose.Typecheck.Definition.Type as T
import qualified Language.Goose.CST.Modules.Declaration as D
import qualified Language.Goose.Typecheck.Definition.Monad as T
import qualified Language.Goose.Typecheck.Modules.Substitution as U
import qualified Control.Monad.State as ST
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
          go (D.Constructor (D.ID (D.Simple "Mutable")) [x]) env = do
            t <- go x env
            return (T.Mutable t)
          go (D.Constructor (D.ID (D.Simple v)) xs) env = do
            aliases <- ST.gets T.aliases
            xs' <- mapM (`go` env) xs
            case M.lookup v aliases of
              Just (T.Forall gens t) -> do
                let env' = M.fromList (zip gens xs')
                return (U.apply env' t)
              Nothing -> return (T.TApp (T.TId v) xs')
          go (D.Constructor v xs) env = do
            t <- go v env
            ts <- mapM (`go` env) xs

            return (T.TApp t ts)
          go (D.Generic v) env = maybe T.fresh return (M.lookup v env)
          go (D.ID (D.Simple v)) _ = do
            aliases <- ST.gets T.aliases
            case M.lookup v aliases of
              Just (T.Forall _ t) -> return t
              Nothing -> return (T.TId v)
          go D.Int _ = return T.Int
          go D.Bool _ = return T.Bool
          go D.Float _ = return T.Float
          go D.Char _ = return T.Char
          go D.Unit _ = return T.Void
          go (D.Function args ret) env = do
            ts <- mapM (`go` env) args
            t <- go ret env
            return $ ts T.:-> t
          go (D.List v) env = T.TApp (T.TId "List") . (:[]) <$> go v env
          go (D.Structure xs) env = do
            ts <- mapM (flip go env . snd) xs
            return (T.TRec (zip (map fst xs) ts))
          go x _ = error $ "to: " ++ show x
  from = undefined