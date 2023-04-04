{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Goose.Module.Monad where
import Language.Goose.CST.Expression
import Language.Goose.CST.Located
import Control.Monad.RWS ( modify, gets, MonadState )
import Control.Monad.Except
import Data.Map

type MonadBundling m = (MonadIO m, MonadError (String, Position) m, MonadState BundlingState m, MonadFail m)

data BundlingState = BundlingState {
  modules      :: [(String, [Located Toplevel])],
  mappings     :: Map String String,
  types        :: Map String String,
  counter      :: Integer,
  currentPaths :: [String]
} deriving Show

emptyBundling :: BundlingState
emptyBundling = BundlingState [] empty empty 0 []

fresh :: MonadBundling m => m Integer
fresh = do
  n <- gets counter
  modify $ \s -> s { counter = n + 1 }
  return n

freshName :: MonadBundling m => String -> m String
freshName name = do
  n <- fresh
  return $ "$m" ++ show n ++ "_" ++ name

freshType :: MonadBundling m => String -> m String
freshType name = do
  n <- fresh
  return $ "$t" ++ show n ++ "_" ++ name
