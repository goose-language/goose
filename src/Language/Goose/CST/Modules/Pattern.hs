module Language.Goose.CST.Modules.Pattern where
import Language.Goose.CST.Literal ( Literal )
import Language.Goose.CST.Located ( Located )
import Language.Goose.CST.Modules.Declaration ( Namespaced )

data Pattern 
  = VariablePattern Namespaced
  | LiteralPattern Literal
  | ListPattern [Located Pattern]
  | StructurePattern [(String, Located Pattern)]
  | WildcardPattern
  | ConstructorPattern Namespaced [Located Pattern]
  deriving Eq

instance Show Pattern where
  show (VariablePattern x) = show x
  show (LiteralPattern l) = show l
  show (ConstructorPattern x ps) = show x ++ " " ++ unwords (map show ps)
  show WildcardPattern = "_"
  show (ListPattern ps) = "[" ++ unwords (map show ps) ++ "]"
  show (StructurePattern ps) = "{" ++ unwords (map (\(x, p) -> x ++ ": " ++ show p) ps) ++ "}"