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
      , output    :: String}
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
    Compile input' libraries' flags' output' -> do
      exists <- doesFileExist input'
      if exists
        then do
          content <- readFile input'
          compileFromString content input' libraries' flags' output'
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

compiler :: Parser CLI
compiler = Compile
  <$> argument str (metavar "INPUT")
  <*> many (strOption (long "includes" <> short 'i' <> metavar "LIBRARIES" <> help "Libraries to link"))
  <*> many (strOption (long "libraries" <> short 'l' <> metavar "LINK LIBRARIES" <> help "Libraries when linking"))
  <*> strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> help "Output file" <> if os == "mingw32" then value "a.exe" else value "a.out")

runner :: Parser CLI
runner = Run
  <$> argument str (metavar "INPUT")
  <*> many (strOption (long "includes" <> short 'i' <> metavar "LIBRARY" <> help "Library to link"))
  <*> many (strOption (long "libraries" <> short 'l' <> metavar "LINK LIBRARY" <> help "Library when linking"))

repl :: Parser CLI
repl = pure Repl