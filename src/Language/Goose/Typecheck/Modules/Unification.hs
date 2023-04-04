{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.Goose.Typecheck.Modules.Unification where
import Language.Goose.Typecheck.Modules.Substitution ( Substitution, Types(free, apply) )
import Language.Goose.Typecheck.Definition.Type
import Language.Goose.Typecheck.Modules.Apply      ( compose )
import Language.Goose.Typecheck.Definition.Monad        ( MonadChecker )

import qualified Data.Map as M
import qualified Control.Monad as CM

variable :: Int -> Type -> Either String Substitution
variable n t
  | t == TVar n = Right M.empty
  | n `elem` free t = Left $ "Occurs check failed in " ++ show t ++ " with " ++ show (TVar n)
  | otherwise = Right $ M.singleton n t

mgu :: MonadChecker m => Type -> Type -> m (Either String Substitution)
mgu (TVar i) t = return $ variable i t
mgu t (TVar i) = return $ variable i t
mgu Int Int = return $ Right M.empty
mgu Bool Bool = return $ Right M.empty
mgu Float Float = return $ Right M.empty
mgu Void Void = return $ Right M.empty
mgu Char Char = return $ Right M.empty
mgu Float Int = return $ Right M.empty
mgu Int Float = return $ Right M.empty
mgu (TApp t1 t2) (TApp t3 t4) = mguMany (t1:t2) (t3:t4)
mgu (TId n) (TId n') = if n == n' 
  then return $ Right M.empty 
  else return $ Left $ "Type mismatch: " ++ show n ++ " and " ++ show n'
mgu (TRec f1) (TRec f2) =
  CM.foldM (\s (f, t) -> case lookup f f2 of
    Just t' -> do
      s' <- mgu t t'
      return $ compose <$> s <*> s'
    Nothing -> return $ Right mempty) (Right M.empty) f1
mgu s1 s2 = return $ Left $ "Type " ++ show s1 ++ " mismatches with type " ++ show s2

mguMany :: MonadChecker m => [Type] -> [Type] -> m (Either String Substitution)
mguMany [] [] = return $ Right M.empty
mguMany (t1:t1s) (t2:t2s) = do
  s1 <- mgu t1 t2
  case s1 of
    Left err -> return $ Left err
    Right s1' -> do
      s2 <- mguMany (apply s1' t1s) (apply s1' t2s)
      return $ compose s1' <$> s2
mguMany t1 t2 = return $ Left $ "Type mismatch: " ++ show t1 ++ " and " ++ show t2

