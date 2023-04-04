module Language.Goose.Typecheck.Modules.Substitution where
import Language.Goose.Typecheck.Definition.Type
import qualified Data.Map as M
import qualified Data.Set as S

type Substitution = M.Map Int Type

class Types a where
  apply :: Substitution -> a -> a
  free  :: a -> S.Set Int