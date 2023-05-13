module Language.Goose.Parser.Modules.Literal where
import qualified Language.Goose.CST.Literal as C
import qualified Language.Goose.Parser.Lexer as L
import qualified Text.Parsec as P
import qualified Data.Functor as F

parseLiteral :: Monad m => L.Parser m C.Literal
parseLiteral = P.choice [
    P.try $ C.Float <$> L.float,
    C.Int <$> L.integer,
    C.Bool <$> (L.reserved "true" F.$> True P.<|> L.reserved "false" F.$> False),
    C.Char <$> L.charLiteral,
    C.String <$> L.stringLiteral,
    C.Unit <$ L.reserved "nil"
  ]
