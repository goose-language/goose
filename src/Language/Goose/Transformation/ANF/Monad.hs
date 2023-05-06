{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Language.Goose.Transformation.ANF.Monad where
import Control.Monad.RWS
import Language.Goose.Transformation.ANF.AST

type MonadANF m = (MonadRWS [(String, String)] [ANFStatement] Int m)

fresh :: MonadANF m => m String
fresh = do
  i <- get
  put (i + 1)
  return $ "$a" ++ show i

createLet :: [(String, ANFExpression)] -> [ANFStatement]
createLet [] = []
createLet ((name', e) : xs) = SLet name' e : createLet xs

createLetTL :: [(String, ANFExpression)] -> [ANFDefinition]
createLetTL [] = []
createLetTL ((name', e) : xs) = DDeclaration name' e : createLetTL xs