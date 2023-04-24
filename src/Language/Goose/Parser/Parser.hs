module Language.Goose.Parser.Parser where
import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as E
import qualified Language.Goose.CST.Expression as C
import qualified Language.Goose.CST.Located as C
import qualified Language.Goose.CST.Annoted as C
import qualified Language.Goose.CST.Literal as C
import qualified Language.Goose.Parser.Lexer as L
import qualified Language.Goose.CST.Modules.Declaration as D

import qualified Language.Goose.Parser.Modules.Toplevel as T
import qualified Language.Goose.Parser.Modules.Declaration as D
import qualified Language.Goose.Parser.Modules.Pattern as P
import qualified Data.Functor as F

import Control.Applicative

type SourceFile = String

parseGoose :: Monad m => SourceFile -> String -> m (Either P.ParseError [C.Located C.Toplevel])
parseGoose = P.runParserT (L.whiteSpace *> P.many (T.parseToplevel parseExpression) <* P.eof) ()

runGoose :: Monad m => SourceFile -> String -> L.Parser m a -> m (Either P.ParseError a)
runGoose file src p = P.runParserT (L.whiteSpace *> p <* P.eof) () file src

makeUnaryOp :: Alternative f => f (a -> a) -> f (a -> a)
makeUnaryOp s = foldr1 (.) . reverse <$> some s

parseExpression :: Monad m => L.Goose m C.Expression
parseExpression = E.buildExpressionParser table parseTerm
  where table = [
            [ E.Postfix $ makeUnaryOp postfix ],
            [ E.Prefix $ makeUnaryOp deref ],
            equalities,
            logicalP,
            [ E.Infix (L.reservedOp "*" >> return (\x@(C.Located (s, _) _) y@(C.Located (_, e) _) -> C.Binary "*" x y C.:>: (s, e))) E.AssocLeft,
              E.Infix (L.reservedOp "/" >> return (\x@(C.Located (s, _) _) y@(C.Located (_, e) _) -> C.Binary "/" x y C.:>: (s, e))) E.AssocLeft],
            [ E.Infix (L.reservedOp "+" >> return (\x@(C.Located (s, _) _) y@(C.Located (_, e) _) -> C.Binary "+" x y C.:>: (s, e))) E.AssocLeft,
              E.Infix (L.reservedOp "-" >> return (\x@(C.Located (s, _) _) y@(C.Located (_, e) _) -> C.Binary "-" x y C.:>: (s, e))) E.AssocLeft]
          ]
        postfix = call P.<|> index P.<|> property
        property = do
          L.reservedOp "."
          name <- L.identifier
          s <- P.getPosition
          return $ \x@(C.Located (pos, _) _) -> C.StructureAccess x name C.:>: (pos, s)
        call = do
          args <- L.parens $ L.commaSep parseExpression
          s <- P.getPosition
          return $ \x@(C.Located (pos, _) _) -> C.Application x args C.:>: (pos, s)
        index = do
          i <- L.brackets parseExpression
          s <- P.getPosition
          return $ \x@(C.Located (pos, _) _) -> C.ListAccess x i C.:>: (pos, s)

        deref = do
          L.reservedOp "*"
          s <- P.getPosition
          return $ \x@(C.Located (pos, _) _) -> C.Dereference x C.:>: (pos, s)

        logicalOp = ["&&", "||"]
        logicalP = map (\op -> E.Infix (L.reservedOp op >> return (\x@(C.Located (s, _) _) y@(C.Located (_, e) _) -> C.Binary op x y C.:>: (s, e))) E.AssocLeft) logicalOp
        equalityOp = ["==", "!=", "<", ">", "<=", ">="]
        equalities = map (\op -> E.Infix (L.reservedOp op >> return (\x@(C.Located (s, _) _) y@(C.Located (_, e) _) -> C.Binary op x y C.:>: (s, e))) E.AssocLeft) equalityOp

parseTerm :: Monad m => L.Goose m C.Expression
parseTerm = P.choice [
    P.try parseFunction,
    L.parens parseExpression,
    parseMutable,
    parseObject,
    parseUpdate,
    parseMatch,
    parseLiteral,
    parseVariable,
    parseList,
    parseLet,
    parseIf,
    parseFor,
    parseWhile,
    parseSequence,
    parseReturn
  ]

parseMutable :: Monad m => L.Goose m C.Expression
parseMutable = L.locate $ do
  L.reserved "mutable"
  C.Mutable <$> parseExpression

parseLiteral :: Monad m => L.Goose m C.Expression
parseLiteral = P.choice [
    L.locate $ C.Literal . C.Int <$> L.integer,
    L.locate $ C.Literal . C.Float <$> L.float,
    L.locate $ C.Literal . C.Bool <$> (L.reserved "true" F.$> True P.<|> L.reserved "false" F.$> False),
    L.locate $ C.Literal . C.Char <$> L.charLiteral,
    stringLiteral parseExpression,
    L.locate $ C.Literal <$> (C.Unit <$ L.reserved "nil")
  ]

stringLiteral :: Monad m => L.Goose m C.Expression -> L.Goose m C.Expression
stringLiteral p = L.lexeme $ do
  s <- P.getPosition
  _ <- P.char '"'
  cs <- P.many1 ((P.try (P.char '{' *> p <* P.char '}')
              >>= \x@(C.Located pos _) ->
                return (C.Application (C.Variable (D.Namespaced ["String"] "from") C.:>: pos) [x] C.:>: pos))
            P.<|> L.locate (P.many1 L.characterChar F.<&> (C.Literal . C.String))) P.<|> return []
  _ <- P.char '"'
  e <- P.getPosition
  return $ case cs of
    [] -> C.Literal (C.String "") C.:>: (s, e)
    (c:cs') -> foldl (\acc x@(C.Located pos _) -> C.Binary "+" acc x C.:>: pos) c cs'

parseMatch :: Monad m => L.Goose m C.Expression
parseMatch = L.locate $ do
  L.reserved "match"
  expr <- parseExpression
  L.reserved "do"
  cases <- L.commaSep parseCase
  L.reserved "end"
  return $ C.Match expr cases
  where parseCase = do
          pattern' <- P.parsePattern
          L.reservedOp "->"
          expr <- parseExpression
          return (pattern', expr)

parseObject :: Monad m => L.Goose m C.Expression
parseObject = L.locate $
  C.Structure <$> L.braces (L.commaSep parseField)
  where parseField = do
          name <- L.identifier
          L.reservedOp ":"
          value <- parseExpression
          return (name, value)

parseReturn :: Monad m => L.Goose m C.Expression
parseReturn = L.locate $ do
  L.reserved "return"
  C.Return <$> parseExpression

parseVariable :: Monad m => L.Goose m C.Expression
parseVariable = L.locate $ C.Variable <$> P.parseNamespaced

parseLet :: Monad m => L.Goose m C.Expression
parseLet = L.locate $ do
  L.reserved "def"
  name <- L.identifier
  ty <- P.optionMaybe $ L.reservedOp ":" *> D.parseDeclaration
  L.reservedOp "="
  value <- parseExpression
  s <- P.getPosition
  body <- P.option (C.Literal C.Unit C.:>: (s, s)) $ L.reserved "in" *> parseExpression
  return $ C.Let (C.Annoted name ty) value body

parseIf :: Monad m => L.Goose m C.Expression
parseIf = L.locate $ do
  L.reserved "if"
  cond <- parseExpression
  L.reserved "then"
  thenBranch <- parseExpression
  elseBranch <- P.optionMaybe $ L.reserved "else" *> parseExpression
  return $ C.If cond thenBranch elseBranch

parseSequence :: Monad m => L.Goose m C.Expression
parseSequence = L.locate $ do
  L.reserved "do"
  exprs <- P.many (parseExpression <* P.optionMaybe L.semi)
  L.reserved "end"
  return $ C.Sequence exprs

parseList :: Monad m => L.Goose m C.Expression
parseList = L.locate $ C.List <$> L.brackets (L.commaSep parseExpression)

parseWhile :: Monad m => L.Goose m C.Expression
parseWhile = L.locate $ do
  L.reserved "while"
  cond <- parseExpression
  L.reserved "do"
  body <- many (parseExpression <* P.optionMaybe L.semi)
  L.reserved "end"
  return $ C.While cond body

parseFunction :: Monad m => L.Goose m C.Expression
parseFunction = L.locate $ do
  L.reserved "fun"
  args <- L.parens (L.commaSep (C.Annoted <$> L.identifier <*> P.optionMaybe (L.reservedOp ":" *> D.parseDeclaration)))
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
  return $ C.Lambda args ret body

parseFor :: Monad m => L.Goose m C.Expression
parseFor = L.locate $ do
  L.reserved "for"
  name <- L.identifier
  ty <- P.optionMaybe $ L.reservedOp ":" *> D.parseDeclaration
  L.reserved "in"
  lst <- parseExpression
  L.reserved "do"
  body <- many (parseExpression <* P.optionMaybe L.semi)
  L.reserved "end"
  return $ C.For (C.Annoted name ty) lst body

parseUpdate :: Monad m => L.Goose m C.Expression
parseUpdate = L.locate $ do
  var <- P.try $ parseVariableUpdate <* L.reservedOp "="
  C.Update var <$> parseExpression

parseVariableUpdate :: Monad m => L.Goose m C.Updated
parseVariableUpdate = P.choice [
    L.locate $ C.VariableUpdate <$> P.parseNamespaced,
    L.parens parseVariableUpdate
  ]
