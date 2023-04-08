module Language.Goose.Parser.Modules.Declaration where
import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as E
import qualified Language.Goose.CST.Modules.Declaration as D
import qualified Language.Goose.Parser.Lexer as L
import qualified Data.Functor as F
import Control.Applicative

makeUnaryOp :: Alternative f => f (a -> a) -> f (a -> a)
makeUnaryOp s = foldr1 (.) . reverse <$> some s

parseDeclaration :: Monad m => L.Parser m D.Declaration
parseDeclaration = E.buildExpressionParser table $ P.choice [
    parsePrimitive,
    parseFunction,
    parseList,
    parseGeneric,
    parseIdentifier
  ]
  where
    table = [
        [ E.Postfix $ makeUnaryOp application ]
      ]
    application = do
      args <- L.brackets $ L.commaSep parseDeclaration
      return $ \x -> D.Constructor x args

parsePrimitive :: Monad m => L.Parser m D.Declaration
parsePrimitive = P.choice [
    L.reserved "int" F.$> D.Int,
    L.reserved "float" F.$> D.Float,
    L.reserved "bool" F.$> D.Bool,
    L.reserved "char" F.$> D.Char,
    L.reserved "nil" F.$> D.Unit,
    L.reserved "string" F.$> D.List D.Char
  ]

parseFunction :: Monad m => L.Parser m D.Declaration
parseFunction = do
  L.reserved "fun"
  args <- L.parens $ L.commaSep (P.optionMaybe (P.try $ L.identifier *> L.reservedOp ":") *> parseDeclaration)
  L.reservedOp ":"
  ret <- parseDeclaration
  return $ D.Function args ret

parseList :: Monad m => L.Parser m D.Declaration
parseList = D.List <$> L.brackets parseDeclaration

parseIdentifier :: Monad m => L.Parser m D.Declaration
parseIdentifier = D.ID <$> parseNamespaced

parseNamespaced :: Monad m => L.Parser m D.Namespaced
parseNamespaced =  P.try (D.Namespaced <$> P.sepBy1 L.identifier (L.reservedOp "::") <*> (L.reservedOp "::" *> L.capitalized))
             P.<|> D.Simple <$> L.capitalized

parseGeneric :: Monad m => L.Parser m D.Declaration
parseGeneric = D.Generic <$> L.lowered