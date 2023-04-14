{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.Goose.Transformation.Closure.Free where
import Language.Goose.Transformation.ANF.AST
import qualified Language.Goose.Typecheck.Definition.AST as T
import qualified Data.Set as S

class Free a where
  free :: a -> S.Set String

instance Free a => Free [a] where
  free = S.unions . map free

instance Free a => Free (Maybe a) where
  free = maybe S.empty free

instance Free a => Free (Either a b) where
  free = either free (const S.empty)

instance Free a => Free (b, a) where
  free = free . snd

instance Free ANFExpression where
  free (EVariable x _) = S.singleton x
  free (EApplication f args) = free f `S.union` free args
  free (EIf cond t f) = free cond `S.union` free t `S.union` free f
  free (EList xs) = free xs
  free (EListAccess arr idx) = free arr `S.union` free idx
  free (ELiteral _) = S.empty
  free (ELambda args body) = freeBody body S.\\ S.fromList args
  free (EStructAccess x _) = free x
  free (EStructure xs) = free xs
  free (EUpdate x e) = free x `S.union` free e
  free (EBinary _ l r) = free l `S.union` free r
  free (EUnary _ e) = free e
  free (EMutable e) = free e
  free (EDereference e) = free e

instance Free ANFStatement where
  free (SLet x e) = free e S.\\ S.singleton x
  free (SExpression e) = free e
  free (SIf cond t f) = free cond `S.union` freeBody t `S.union` freeBody f
  free (SWhile cond body) = free cond `S.union` freeBody body
  free (SUpdate x e) = free x `S.union` free e
  free (SFor x e body) = (free e `S.union` freeBody body) S.\\ S.singleton x
  free (SBlock body) = freeBody body
  free (SReturn e) = free e
  free SBreak = S.empty
  free SContinue = S.empty
  free (SMatch e cases) = free e `S.union` S.unions (map (\(p, b) -> freeBodyExcl b (free p)) cases)

instance Free T.Pattern where
  free (T.PVariable x _) = S.singleton x
  free (T.PList xs) = free xs
  free (T.PStructure xs) = free xs
  free (T.PWildcard) = S.empty
  free (T.PConstructor name pats) = free pats `S.union` S.singleton name
  free (T.PLiteral _) = S.empty

instance Free ANFDefinition where
  free (DDeclaration x e) = free e S.\\ S.singleton x
  free (DFunction name args body) = free body S.\\ S.fromList (name:args)
  free (DExtern _) = S.empty
  free (DDeclare _) = S.empty


freeBody :: [ANFStatement] -> S.Set String
freeBody body = fst $ foldl (\(acc, excluded) -> \case
    SLet n e -> 
      (acc `S.union` free e S.\\ S.singleton n, excluded `S.union` S.singleton n)
    x -> 
      (acc `S.union` free x S.\\ excluded, excluded)) (S.empty, S.empty) body

freeBodyExcl :: [ANFStatement] -> S.Set String -> S.Set String
freeBodyExcl body excl = fst $ foldl (\(acc, excluded) -> \case
    SLet n e -> 
      (acc `S.union` free e S.\\ S.singleton n, excluded `S.union` S.singleton n)
    x -> 
      (acc `S.union` free x S.\\ excluded, excluded)) (S.empty, excl) body