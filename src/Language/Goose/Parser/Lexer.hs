module Language.Goose.Parser.Lexer where
import Text.Parsec
import Language.Goose.CST.Located
import qualified Text.Parsec.Token as Token

type Goose m a = Parser m (Located a)
type Parser m a  = ParsecT String () m a

languageDef :: Monad m => Token.GenLanguageDef String u m
languageDef =
  Token.LanguageDef {  
              Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.nestedComments  = True
            , Token.caseSensitive   = True
            , Token.identStart      = letter <|> char '_'
            , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
            , Token.opStart         = Token.opLetter languageDef
            , Token.identLetter     = alphaNum <|> char '_' <|> char '\''
            , Token.reservedNames   = ["enum", "end", "public", "module", "def", "for", "in", "do", "while", "break", "if", "then", "else", "import", "extern", "return", "declare", "fun", "match", "type"]
            , Token.reservedOpNames = ["(", ")", "*", "+", "-", "/", "{", "}", "[", "]", "<", ">", "=", "->", "::"] }

lexer :: Monad m => Token.GenTokenParser String u m
lexer = Token.makeTokenParser languageDef

identifier :: Monad m => Parser m String
identifier = Token.identifier lexer

reserved :: Monad m => String -> Parser m ()
reserved = Token.reserved lexer

reservedOp :: Monad m => String -> Parser m ()
reservedOp = Token.reservedOp lexer

parens :: Monad m => Parser m a -> Parser m a
parens = Token.parens lexer

charLiteral :: Monad m => Parser m Char
charLiteral = Token.charLiteral lexer

stringLiteral :: Monad m => Parser m String
stringLiteral = Token.stringLiteral lexer

integer :: Monad m => Parser m Integer
integer = Token.integer lexer

float :: Monad m => Parser m Double
float = Token.float lexer

whiteSpace :: Monad m => Parser m ()
whiteSpace = Token.whiteSpace lexer

comma :: Monad m => Parser m String
comma = Token.comma lexer

commaSep :: Monad m => Parser m a -> Parser m [a]
commaSep = Token.commaSep lexer

semi :: Monad m => Parser m String
semi = Token.semi lexer

braces :: Monad m => Parser m a -> Parser m a
braces = Token.braces lexer

brackets :: Monad m => Parser m a -> Parser m a
brackets = Token.brackets lexer

locate :: Monad m => Parser m a -> Goose m a
locate p = do
  start <- getPosition
  r <- p
  end <- getPosition
  return (r :>: (start, end))

lexeme :: Monad m => Parser m a -> Parser m a
lexeme = Token.lexeme lexer 

lowered :: Monad m => Parser m String
lowered = lexeme $ do
  c <- lower
  cs <- many alphaNum
  return (c:cs)

capitalized :: Monad m => Parser m String
capitalized = lexeme $ do
  c <- upper
  cs <- many alphaNum
  return (c:cs)