module Language.Goose.Parser.Modules.Declaration where
import qualified Text.Parsec as P
import qualified Language.Goose.CST.Modules.Declaration as D
import qualified Language.Goose.Parser.Lexer as L
import qualified Data.Functor as F

parseDeclaration :: Monad m => L.Parser m D.Declaration
parseDeclaration = P.choice [
    parsePrimitive,
    parseList,
    parseGeneric,
    parseIdentifier
  ]

parsePrimitive :: Monad m => L.Parser m D.Declaration
parsePrimitive = P.choice [
    L.reserved "int" F.$> D.Int,
    L.reserved "float" F.$> D.Float,
    L.reserved "bool" F.$> D.Bool,
    L.reserved "char" F.$> D.Char,
    L.reserved "nil" F.$> D.Unit
  ]

parseList :: Monad m => L.Parser m D.Declaration
parseList = D.List <$> L.brackets parseDeclaration

parseIdentifier :: Monad m => L.Parser m D.Declaration
parseIdentifier = D.ID <$> parseNamespaced

parseNamespaced :: Monad m => L.Parser m D.Namespaced
parseNamespaced =  P.try (D.Namespaced <$> P.sepBy1 L.identifier (L.reservedOp "::") <*> (L.reservedOp "::" *> L.capitalized))
             P.<|> D.Simple <$> L.capitalized

parseGeneric :: Monad m => L.Parser m D.Declaration
parseGeneric = D.Generic <$> L.lowered