module CLI.CLI where

import Options.Applicative
import System.Info
import System.Directory.Internal.Prelude (exitFailure)
import System.Directory (doesFileExist)
import CLI.Phases (compileFromString, getANFDefinitions)
import CLI.REPL (runREPL)
import Language.Goose.Parser.Parser (parseGoose)
import Language.Goose.LLVM.BuildLibrary (buildAndRun)
import Control.Monad (forM_)
import qualified Log.Error as L

data CLI =
    Compile
      { input     :: String
      , libraries :: [String]
      , flags     :: [String]
      , output    :: String
      , target    :: String }
  | Run
      { input     :: String
      , libraries :: [String]
      , flags     :: [String] }
  | Repl
  deriving Show

splitOnSep :: String -> Char -> [String]
splitOnSep [] _ = []
splitOnSep (x:xs) sep
  | x == sep = splitOnSep xs sep
  | otherwise = let (word, rest) = break (== sep) (x:xs) in word : splitOnSep rest sep

runCLI :: IO ()
runCLI = do
  cli' <- customExecParser p opts
  case cli' of
    Compile input' libraries' flags' output' target' -> do
      exists <- doesFileExist input'
      if exists
        then do
          content <- readFile input'
          compileFromString content input' libraries' flags' output' target'
        else putStrLn "File does not exist" *> exitFailure
    Run input' libraries' flags' -> do
      x <- doesFileExist input'
      if x
        then do
          content <- readFile input'
          ast <- parseGoose input' content
          case ast of
            Left err -> L.printParseError err input' content
            Right ast' -> do
              anf <- getANFDefinitions content ast' input'
              case anf of
                Nothing -> return ()
                Just anf' -> do
                  res <- buildAndRun anf' libraries' flags'
                  forM_ res putStrLn
        else putStrLn "File does not exist" *> exitFailure
    Repl -> runREPL
  where
    opts = info (cli <**> helper)
      ( fullDesc
     <> header "Goose compiler" )
    p = prefs showHelpOnEmpty

cli :: Parser CLI
cli = subparser
  ( command "compile" (info compiler (progDesc "Compile a file"))
 <> command "run" (info runner (progDesc "Run a file"))
 <> command "repl" (info repl (progDesc "Run the REPL")) )

-- arm-none-eabi
-- armv7a-none-eabi
-- arm-linux-gnueabihf 
-- arm-none-linux-gnueabi
-- i386-pc-linux-gnu 
-- x86_64-apple-darwin10
-- i686-w64-windows-gnu # same as i686-w64-mingw32
-- x86_64-pc-linux-gnu # from ubuntu 64 bit
-- x86_64-unknown-windows-cygnus # cygwin 64-bit
-- x86_64-w64-windows-gnu # same as x86_64-w64-mingw32
-- i686-pc-windows-gnu # MSVC
-- x86_64-pc-windows-gnu # MSVC 64-BIT

compiler :: Parser CLI
compiler = Compile
  <$> argument str (metavar "INPUT")
  <*> many (strOption (long "includes" <> short 'i' <> metavar "LIBRARIES" <> help "Libraries to link"))
  <*> many (strOption (long "libraries" <> short 'l' <> metavar "LINK LIBRARIES" <> help "Libraries when linking"))
  <*> strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> help "Output file" <> if os == "mingw32" then value "a.exe" else value "a.out")
  <*> strOption (long "target" <> metavar "TARGET OS" <> help "Cross compile to a different OS" <> if os == "mingw32" then value "x86_64-w64-windows-gnu" else if os == "darwin" then value "x86_64-apple-darwin10" else value "i386-pc-linux-gnu ")

runner :: Parser CLI
runner = Run
  <$> argument str (metavar "INPUT")
  <*> many (strOption (long "includes" <> short 'i' <> metavar "LIBRARY" <> help "Library to link"))
  <*> many (strOption (long "libraries" <> short 'l' <> metavar "LINK LIBRARY" <> help "Library when linking"))

repl :: Parser CLI
repl = pure Repl