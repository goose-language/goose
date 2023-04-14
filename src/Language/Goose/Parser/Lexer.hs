{-# LANGUAGE FlexibleContexts #-}
module Language.Goose.Parser.Lexer where
import Text.Parsec
import Language.Goose.CST.Located
import qualified Text.Parsec.Token as Token
import Data.Char

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
            , Token.reservedNames   = ["enum", "end", "public", "module", "def", "for", "in", "do", "while", "break", "if", "then", "else", "import", "extern", "return", "declare", "fun", "match", "type", "mutable"]
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

-- stringLiteral :: Monad m => Parser m String
-- stringLiteral = Token.stringLiteral lexer

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

stringLiteral :: Monad m => Parser m String
stringLiteral = Token.stringLiteral lexer

decimal :: Monad m => Parser m Integer
decimal = Token.decimal lexer

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

characterChar :: Monad m => Parser m Char
characterChar = charLetter <|> charEscape

charEscape :: Monad m => Parser m Char
charEscape = do{ _ <- char '\\'; escapeCode }

charLetter :: Monad m => Parser m Char
charLetter = satisfy (\c -> (c /= '\"') && (c /= '{') && (c /= '\\') && (c > '\026'))

escapeCode :: Monad m => Parser m Char
escapeCode = charEsc <|> charNum <|> charAscii <|> charControl

charControl :: Monad m => Parser m Char
charControl = do
  _ <- char '^'
  ; code <- upper
  ; return (toEnum (fromEnum code - fromEnum 'A' + 1))

charNum :: Monad m => Parser m Char
charNum = do
  code <- decimal <|> do { _ <- char 'o'; number 8 octDigit }
                  <|> do { _ <- char 'x'; number 16 hexDigit }
  if code > 0x10FFFF
    then fail "invalid escape sequence"
    else return (toEnum (fromInteger code))
                        
number :: Monad m => Integer -> Parser m Char -> Parser m Integer
number base baseDigit = do
  digits <- many1 baseDigit
  let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
  seq n (return n)
    
charEsc :: Monad m => Parser m Char
charEsc = choice (map parseEsc escMap)
  where
    parseEsc (c,code) = do{ _ <- char c; return code }

charAscii :: Monad m => Parser m Char
charAscii = choice (map parseAscii asciiMap)
  where
    parseAscii (asc,code) = try (do{ _ <- string asc; return code })

escMap :: [(Char, Char)]
escMap = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")

asciiMap :: [(String, Char)]
asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

ascii2codes :: [String]
ascii2codes = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
               "FS","GS","RS","US","SP"]
ascii3codes :: [String]
ascii3codes = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
               "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
               "CAN","SUB","ESC","DEL"]

ascii2 :: String
ascii2 = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
          '\EM','\FS','\GS','\RS','\US','\SP']
ascii3 :: String
ascii3 = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
          '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
          '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']