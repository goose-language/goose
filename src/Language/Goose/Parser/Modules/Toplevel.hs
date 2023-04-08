module Language.Goose.Parser.Modules.Toplevel where
import qualified Text.Parsec as P
import qualified Language.Goose.CST.Expression as C
import qualified Language.Goose.CST.Annoted as C
import qualified Language.Goose.CST.Located as C
import qualified Language.Goose.Parser.Lexer as L

import qualified Language.Goose.Parser.Modules.Declaration as D

-- Toplevel parser entry
parseToplevel :: Monad m => L.Goose m C.Expression -> L.Goose m C.Toplevel
parseToplevel parseExpression = P.choice [
    parseEnumeration,
    parseModule parseExpression,
    parseDeclare,
    parseFunction parseExpression,
    parsePublic parseExpression,
    parseExtern,
    P.try parseImportAs,
    parseImport
  ]

parseEnumeration :: Monad m => L.Goose m C.Toplevel
parseEnumeration = L.locate $ do
  L.reserved "enum"
  name <- L.capitalized
  generics <- P.option [] $ L.brackets (L.commaSep L.lowered)
  values <- P.many (C.Annoted <$> L.identifier <*> P.option [] (L.parens (L.commaSep D.parseDeclaration)))
  L.reserved "end"
  return $ C.Enumeration name generics values

parseFunction :: Monad m => L.Goose m C.Expression -> L.Goose m C.Toplevel
parseFunction parseExpression = L.locate $ do
  L.reserved "def"
  name <- L.identifier
  generics <- P.option [] $ L.brackets (L.commaSep L.lowered)
  args <- L.parens (L.commaSep (C.Annoted <$> L.identifier <*> (P.optionMaybe $ L.reservedOp ":" *> D.parseDeclaration)))
  (ret, body) <- P.choice [
      P.try $ do
        L.reservedOp ":"
        ret <- D.parseDeclaration
        s <- P.getPosition
        L.reservedOp "do"
        exprs <- P.many (parseExpression <* P.optionMaybe L.semi)
        L.reserved "end"
        e <- P.getPosition
        return (Just ret, C.Located (s, e) (C.Sequence exprs)),
      do
        body <- parseExpression
        return (Nothing, body)
    ]
  return $ C.Function (C.Annoted name ret) generics args body

parsePublic :: Monad m => L.Goose m C.Expression -> L.Goose m C.Toplevel
parsePublic parseExpression = L.locate $ do
  L.reserved "public"
  body <- parseToplevel parseExpression
  return $ C.Public body

parseDeclare :: Monad m => L.Goose m C.Toplevel
parseDeclare = L.locate $ do
  L.reserved "declare"
  name <- L.identifier
  gens <- P.option [] $ L.brackets $ L.commaSep (P.many1 P.lower)
  args <- P.option [] $ L.parens (L.commaSep (P.optionMaybe (L.identifier >> L.reservedOp ":") *> D.parseDeclaration))
  L.reservedOp ":"
  value <- D.parseDeclaration
  return $ C.Declare name gens args value

parseModule :: Monad m => L.Goose m C.Expression -> L.Goose m C.Toplevel
parseModule parseExpression = L.locate $ do
  L.reserved "module"
  name <- L.identifier
  body <- P.many $ parseToplevel parseExpression
  L.reserved "end"
  return $ C.Namespace name body

parseExtern :: Monad m => L.Goose m C.Toplevel
parseExtern = L.locate $ do
  L.reserved "extern"
  name <- L.identifier
  gens <- L.brackets $ L.commaSep (P.many1 P.lower)
  args <- L.parens (L.commaSep (P.optionMaybe (L.identifier <* L.reservedOp ":") *> D.parseDeclaration))
  L.reservedOp ":"
  ret <- D.parseDeclaration
  return $ C.Extern name gens args ret

parseImport :: Monad m => L.Goose m C.Toplevel
parseImport = L.locate $ do
  L.reserved "import"
  name <- L.stringLiteral
  return $ C.Import name

parseImportAs :: Monad m => L.Goose m C.Toplevel
parseImportAs = L.locate $ do
  L.reserved "import"
  name <- L.stringLiteral
  L.reserved "as"
  alias <- L.identifier
  return $ C.ImportAs name alias