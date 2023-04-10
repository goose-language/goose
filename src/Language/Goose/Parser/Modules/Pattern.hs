module Language.Goose.Parser.Modules.Pattern where
import qualified Text.Parsec as P
import qualified Language.Goose.CST.Modules.Pattern as C
import qualified Language.Goose.Parser.Lexer as L
import qualified Language.Goose.CST.Modules.Declaration as D
import qualified Language.Goose.Parser.Modules.Literal as L

parseNamespaced :: Monad m => L.Parser m D.Namespaced
parseNamespaced = P.try (do
    names <- P.sepBy1 L.identifier (L.reservedOp "::")
    case names of 
      [name] -> return $ D.Simple name
      _ -> return $ D.Namespaced (init names) (last names))
  P.<|> D.Simple <$> L.identifier

parsePattern :: Monad m => L.Goose m C.Pattern
parsePattern = P.choice [
    parseWildcard,
    P.try parseConstructor,
    parseVariable,
    parseLiteral,
    parseList,
    parseStructure
  ]

parseVariable :: Monad m => L.Goose m C.Pattern
parseVariable = L.locate $ do
  name <- parseNamespaced
  return $ C.VariablePattern name

parseLiteral :: Monad m => L.Goose m C.Pattern
parseLiteral = L.locate $ do
  lit <- L.parseLiteral
  return $ C.LiteralPattern lit

parseList :: Monad m => L.Goose m C.Pattern
parseList = L.locate $ do
  items <- L.brackets $ P.many (parsePattern <* P.optionMaybe L.comma)
  return $ C.ListPattern items

parseStructure :: Monad m => L.Goose m C.Pattern
parseStructure = L.locate $ do
  items <- L.braces $ P.many ((,) <$> (L.identifier <* L.reservedOp ":") <*> parsePattern <* P.optionMaybe L.comma)
  return $ C.StructurePattern items

parseWildcard :: Monad m => L.Goose m C.Pattern
parseWildcard = L.locate $ do
  L.reservedOp "_"
  return $ C.WildcardPattern

parseConstructor :: Monad m => L.Goose m C.Pattern
parseConstructor = L.locate $ do
  name <- parseNamespaced
  args <- L.parens $ L.commaSep parsePattern
  return $ C.ConstructorPattern name args
