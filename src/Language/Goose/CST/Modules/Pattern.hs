module Language.Goose.CST.Modules.Pattern where
import Language.Goose.CST.Literal ( Literal )
import Language.Goose.CST.Located ( Located )

data Pattern
  = Variable String
  | Literal Literal
  | Constructor String [Located Pattern]
  | Wildcard
  | As String (Located Pattern)
  deriving Eq

instance Show Pattern where
  show (Variable x) = x
  show (Literal l) = show l
  show (Constructor x ps) = x ++ " " ++ unwords (map show ps)
  show Wildcard = "_"
  show (As x p) = x ++ " as " ++ show p