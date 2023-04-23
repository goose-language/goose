module Language.Goose.Typecheck.Definition.Free where
import qualified Data.Set as S
import qualified Language.Goose.CST.Annoted as C
import Language.Goose.Typecheck.Definition.AST

class Free a where
  free :: a -> S.Set String

instance Free Toplevel where
  free (Function name args _ body) = free body S.\\ (S.fromList (map C.annotedName args) `S.union` S.singleton name)
  free (Declaration name _ body) = free body S.\\ S.singleton name
  free (Declare _ _) = S.empty

instance Free Expression where
  free (Literal _) = S.empty
  free (Variable name _) = S.singleton name
  free (Application name args) = S.unions (map free args) `S.union` free name
  free (Let (C.Annoted name _) expr body) = free expr `S.union` (free body S.\\ S.singleton name)
  free (Return expr) = free expr
  free (If cond then' else') = free cond `S.union` free then' `S.union` maybe S.empty free else'
  free (While cond body) = free cond `S.union` S.unions (map free body)
  free (For (C.Annoted name _) expr body) = free expr `S.union` (S.unions (map free body) S.\\ S.singleton name)
  free (Update name expr) = free expr `S.union` free name
  free (Sequence exprs) = S.unions (map free exprs)
  free (Binary _ lhs rhs) = free lhs `S.union` free rhs
  free (Lambda args body) = free body S.\\ S.fromList (map C.annotedName args)
  free (List exprs) = S.unions (map free exprs)
  free (ListAccess e i) = free e `S.union` free i
  free (Structure fields) = S.unions (map (free . snd) fields)
  free (StructureAccess e _) = free e
  free _ = error "Not implemented"

instance Free Updated where
  free (VariableUpdate n _) = S.singleton n
  free (ListUpdate u i) = free u `S.union` free i
  free (StructureUpdate u _) = free u
