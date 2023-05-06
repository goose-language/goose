module Language.Goose.Parser.Modules.Toplevel where
import qualified Text.Parsec as P
import qualified Language.Goose.CST.Expression as C
import qualified Language.Goose.CST.Annoted as C
import qualified Language.Goose.CST.Located as C
import qualified Language.Goose.Parser.Lexer as L

import qualified Language.Goose.Parser.Modules.Declaration as D

-- Toplevel parser entry
parseToplevel :: Monad m => L.Goose m C.Expression -> L.Goose m C.Toplevel
parseToplevel p = P.choice [
    parseType,
    parseEnumeration,
    parseModule p,
    P.try parseEnumDeclaration,
    parseDeclare,
    P.try $ parseDeclaration p,
    parseFunction p,
    parsePublic p,
    parseImport,
    parseExpression p
  ]

parseExpression :: Monad m => L.Goose m C.Expression -> L.Goose m C.Toplevel
parseExpression p = L.locate $ C.Expression <$> p

parseEnumDeclaration :: Monad m => L.Goose m C.Toplevel
parseEnumDeclaration = L.locate $ do
  L.reserved "declare"
  L.reserved "enum"
  name <- L.capitalized
  generics <- P.option [] $ L.brackets (L.commaSep L.lowered)
  return $ C.EnumDeclare name generics

parseEnumeration :: Monad m => L.Goose m C.Toplevel
parseEnumeration = L.locate $ do
  L.reserved "enum"
  name <- L.capitalized
  generics <- P.option [] $ L.brackets (L.commaSep L.lowered)
  values <- P.many (C.Annoted <$> L.identifier <*> P.option [] (L.parens (L.commaSep D.parseDeclaration)))
  L.reserved "end"
  return $ C.Enumeration name generics values

parseType :: Monad m => L.Goose m C.Toplevel
parseType = L.locate $ do
  L.reserved "type"
  name <- L.capitalized
  generics <- P.option [] $ L.brackets (L.commaSep L.lowered)
  L.reservedOp "="
  C.Type name generics <$> D.parseDeclaration

parseFunction :: Monad m => L.Goose m C.Expression -> L.Goose m C.Toplevel
parseFunction p = L.locate $ do
  L.reserved "def"
  name <- L.identifier
  generics <- P.option [] $ L.brackets (L.commaSep L.lowered)
  args <- L.parens (L.commaSep (C.Annoted <$> L.identifier <*> P.optionMaybe (L.reservedOp ":" *> D.parseDeclaration)))
  (ret, body) <- P.choice [
      P.try $ do
        L.reservedOp ":"
        ret <- D.parseDeclaration
        s <- P.getPosition
        L.reservedOp "do"
        exprs <- P.many (p <* P.optionMaybe L.semi)
        L.reserved "end"
        e <- P.getPosition
        return (Just ret, C.Located (s, e) (C.Sequence exprs)),
      do
        body <- p
        return (Nothing, body)
    ]
  return $ C.Function (C.Annoted name ret) generics args body

parsePublic :: Monad m => L.Goose m C.Expression -> L.Goose m C.Toplevel
parsePublic p = L.locate $ do
  L.reserved "public"
  body <- parseToplevel p
  return $ C.Public body

parseDeclaration :: Monad m => L.Goose m C.Expression -> L.Goose m C.Toplevel
parseDeclaration expr = L.locate $ do
  L.reserved "def"
  name <- L.identifier
  args <- P.optionMaybe $ L.reserved ":" *> D.parseDeclaration
  L.reservedOp "="
  C.Declaration (C.Annoted name args) <$> expr

parseDeclare :: Monad m => L.Goose m C.Toplevel
parseDeclare = L.locate $ do
  L.reserved "declare"
  name <- L.identifier
  gens <- P.option [] $ L.brackets $ L.commaSep (P.many1 P.lower)
  args <- P.optionMaybe $ L.parens (L.commaSep (P.optionMaybe (L.identifier >> L.reservedOp ":") *> D.parseDeclaration))
  L.reservedOp ":"
  C.Declare name gens args <$> D.parseDeclaration

parseModule :: Monad m => L.Goose m C.Expression -> L.Goose m C.Toplevel
parseModule p = L.locate $ do
  L.reserved "module"
  name <- L.identifier
  body <- P.many $ parseToplevel p
  L.reserved "end"
  return $ C.Namespace name body

parseImport :: Monad m => L.Goose m C.Toplevel
parseImport = L.locate $ do
  L.reserved "import"
  C.Import <$> L.stringLiteral
