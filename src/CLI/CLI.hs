module CLI.CLI where

import Options.Applicative
import System.Info
import System.Directory.Internal.Prelude (exitFailure)
import System.Directory (doesFileExist)
import CLI.Phases (compileFromString)

data CLI =
    Compile
      { input     :: String
      , libraries :: String
      , flags     :: String
      , output    :: String}
  | Run
      { input     :: String
      , libraries :: String }
  | Repl
      { libraries :: String }
  deriving Show

splitOnSep :: String -> Char -> [String]
splitOnSep [] _ = []
splitOnSep (x:xs) sep
  | x == sep = splitOnSep xs sep
  | otherwise = let (word, rest) = break (== sep) (x:xs) in word : splitOnSep rest sep

runCLI :: IO ()
runCLI = do
  cli <- customExecParser p opts
  case cli of
    Compile input libraries flags output -> do
      exists <- doesFileExist input
      if exists
        then do
          content <- readFile input
          compileFromString content input (splitOnSep libraries ',') (splitOnSep flags ',') output
        else putStrLn "File does not exist" *> exitFailure
    Run input libraries -> do
      putStrLn "Run"
      putStrLn $ "Input: " ++ input
      putStrLn $ "Libraries: " ++ show (splitOnSep libraries ',')
    Repl libraries -> do
      putStrLn "Repl"
      putStrLn $ "Libraries: " ++ show (splitOnSep libraries ',')
  where
    opts = info (cli <**> helper)
      ( fullDesc
     <> header "Goose compiler" )
    p = prefs showHelpOnEmpty

cli :: Parser CLI
cli = subparser
  ( command "compile" (info compiler (progDesc "Compile a Goose program")) )

compiler :: Parser CLI
compiler = Compile
  <$> argument str (metavar "INPUT")
  <*> strOption (long "includes" <> short 'i' <> metavar "LIBRARIES" <> help "Libraries to link" <> value "")
  <*> strOption (long "libraries" <> short 'l' <> metavar "LINK LIBRARIES" <> help "Libraries when linking" <> value "")
  <*> strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> help "Output file" <> if os == "mingw32" then value "a.exe" else value "a.out")

runner :: Parser CLI
runner = Run
  <$> argument str (metavar "INPUT")
  <*> strOption (long "includes" <> short 'i' <> metavar "LIBRARY" <> help "Library to link")

repl :: Parser CLI
repl = Repl
  <$> strOption (long "includes" <> short 'i' <> metavar "LIBRARY" <> help "Library to link")