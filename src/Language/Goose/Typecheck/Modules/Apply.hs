{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Goose.Typecheck.Modules.Apply where
import qualified Language.Goose.Typecheck.Definition.Type as T
import Language.Goose.Typecheck.Definition.AST
import Language.Goose.Typecheck.Modules.Substitution
import Language.Goose.CST.Located
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.Goose.CST.Annoted as C

type Variables = M.Map String T.Scheme
type Environment = (Variables, Variables)

data ConstraintConstructor
  = T.Type :~: T.Type
  | Field {- named -} String {- of -} T.Type {- in -} T.Type
type Constraint = (ConstraintConstructor, Position)

instance {-# OVERLAPPING #-} Types Constraint where
  free (c, _) = free c
  apply s (c, pos) = (apply s c, pos)

instance Show ConstraintConstructor where 
  show (t1 :~: t2) = show t1 ++ " ~ " ++ show t2
  show (Field f t1 t2) = show t1 ++ "." ++ f ++ " ~ " ++ show t2

instance {-# OVERLAPS #-} Show Constraint where
  show (c, _) = show c

instance Types ConstraintConstructor where
  free (t1 :~: t2) = free t1 `S.union` free t2
  free (Field _ t1 t2) = free t1 `S.union` free t2

  apply s (t1 :~: t2) = apply s t1 :~: apply s t2
  apply s (Field f t1 t2) = Field f (apply s t1) (apply s t2)

compose :: Substitution -> Substitution -> Substitution
compose s1 s2 = M.map (apply s1) s2 `M.union` M.map (apply s2) s1

instance Types T.Type where
  free (T.TVar i) = S.singleton i
  free T.Int = S.empty
  free (T.TApp n xs) = free n `S.union` free xs
  free T.Bool = S.empty
  free T.Float = S.empty
  free T.Void = S.empty
  free (T.TId _) = S.empty
  free T.Char = S.empty
  free (T.TRec xs) = free xs

  apply s (T.TVar i) = case M.lookup i s of
    Just t -> t
    Nothing -> T.TVar i
  apply s (T.TApp n xs) = T.TApp (apply s n) $ apply s xs
  apply _ T.Int = T.Int
  apply _ T.Bool = T.Bool
  apply _ T.Float = T.Float
  apply _ T.Void = T.Void
  apply _ (T.TId s) = T.TId s
  apply _ T.Char = T.Char
  apply s (T.TRec xs) = T.TRec $ apply s xs

instance Types Char where
  free _ = S.empty
  apply _ = id

instance Types a => Types [a] where
  free = foldr (S.union . free) S.empty
  apply s = map (apply s)

instance Types T.Scheme where
  free (T.Forall v t) = free t S.\\ S.fromList v
  apply s (T.Forall v t) = T.Forall v (apply (foldr M.delete s v) t)

instance Types Variables where
  free = free . M.elems
  apply s = M.map (apply s)  

instance Types a => Types (Located a) where
  free (a :>: _) = free a
  free _ = S.empty

  apply s (a :>: pos) = apply s a :>: pos
  apply _ x = x

instance Types a => Types (Maybe a) where
  free = maybe S.empty free
  apply s = fmap (apply s)

instance Types a => Types (Either a b) where
  free = either free (const S.empty)
  apply s = either (Left . apply s) Right

instance (Types a, Types b) => Types (a, b) where
  free = S.union <$> free . fst <*> free . snd
  apply s (a, b) = (apply s a, apply s b)

instance Types Position where
  free _ = S.empty
  apply _ = id

instance Types Expression where
  free _ = undefined

  apply s (Variable name t) = Variable name $ apply s t
  apply s (Application f xs) = Application (apply s f) $ apply s xs
  apply s (Lambda name e) = Lambda (apply s name) $ apply s e
  apply s (Let name e1 e2) = Let (apply s name) (apply s e1) $ apply s e2
  apply s (Match e cs) = Match (apply s e) $ apply s cs
  apply s (Sequence xs) = Sequence $ apply s xs
  apply s (List es) = List $ apply s es
  apply s (ListAccess e i) = ListAccess (apply s e) $ apply s i
  apply s (While e1 e2) = While (apply s e1) $ apply s e2
  apply s (For name e1  e3) = For (apply s name) (apply s e1) $ apply s e3
  apply s (If e1 e2 e3) = If (apply s e1) (apply s e2) $ apply s e3
  apply s (Update x e) = Update x $ apply s e
  apply _ (Literal l) = Literal l
  apply s (Return e) = Return $ apply s e
  apply s (Binary op e1 e2) = Binary op (apply s e1) $ apply s e2
  apply s (Structure fields) = Structure $ apply s fields
  apply s (StructureAccess e f) = StructureAccess (apply s e) f
  apply s (Mutable e) = Mutable $ apply s e
  apply s (Dereference e) = Dereference $ apply s e

instance Types Updated where
  free _ = undefined

  apply s (VariableUpdate name e) = VariableUpdate name $ apply s e
  apply s (ListUpdate x y) = ListUpdate (apply s x) $ apply s y
  apply s (StructureUpdate x f) = StructureUpdate (apply s x) f

instance Types Pattern where
  free _ = undefined

  apply s (PVariable x t) = PVariable x $ apply s t
  apply s (PConstructor name ts) = PConstructor name (apply s ts)
  apply _ PWildcard = PWildcard
  apply _ (PLiteral l) = PLiteral l
  apply s (PList ps) = PList $ apply s ps
  apply s (PStructure fs) = PStructure $ apply s fs

instance Types Toplevel where
  free _ = undefined

  apply s (Function name args ret body) = Function name (apply s args) (apply s ret) (apply s body)
  apply s (Declaration name ty expr) = Declaration name (apply s ty) (apply s expr)
  apply s (Declare name ty) = Declare (apply s name) (apply s ty)

instance Types a => Types (C.Annoted a) where
  free = free . C.annotedType
  apply s (C.Annoted x t) = C.Annoted x (apply s t)