{-# LANGUAGE FlexibleContexts #-}
module Language.Goose.Typecheck.ConstraintSolver where
import Language.Goose.Typecheck.Definition.Monad
import Language.Goose.Typecheck.Modules.Substitution
import Language.Goose.Typecheck.Modules.Apply
import Language.Goose.Typecheck.Modules.Unification
import Language.Goose.Typecheck.Definition.Type

import qualified Control.Monad.Except as E
import qualified Data.Bifunctor as BF

solve :: MonadChecker m => [Constraint] -> m Substitution
solve [] = return mempty
solve ((t1 :~: t2, pos):xs) = do
  s1 <- mgu t1 t2
  case s1 of
    Left err -> E.throwError (err, Nothing, pos)
    Right s1' -> do
      s2 <- solve $ map (BF.first (apply s1')) xs
      return $ s1' `compose` s2
solve ((Field f t1 t2, pos):xs) = do
  case t2 of
    TRec fields -> do
      case lookup f fields of
        Just t -> do
          s1 <- mgu t1 t
          case s1 of
            Left err -> E.throwError (err, Nothing, pos)
            Right s1' -> do
              s2 <- solve $ map (BF.first (apply s1')) xs
              return $ s1' `compose` s2
        Nothing -> E.throwError ("Field " ++ f ++ " not found in record type", Nothing, pos)
    TVar _ -> do
      s1 <- mgu t2 (TRec [(f, t1)])
      case s1 of
        Left err -> E.throwError (err, Nothing, pos)
        Right s1' -> do
          s2 <- solve $ map (BF.first (apply s1')) xs
          return $ s1' `compose` s2
    _ -> E.throwError ("Expected record type, got " ++ show t2, Nothing, pos)