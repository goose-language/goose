{-# LANGUAGE FlexibleContexts #-}
module CLI.REPL where
import Language.Goose.Parser.Parser ( parseExpression, runGoose )
import Language.Goose.CST.Expression (Toplevel (Function), Expression (Sequence, Let))
import Language.Goose.CST.Located (Located)
import Language.Goose.Parser.Modules.Toplevel (parseToplevel)
import Language.Goose.Parser.Lexer (parseEither, reservedWords)
import qualified Text.Parsec as P
import Text.Parsec (ParseError)
import CLI.Phases (getANFDefinitions)
import qualified Language.Goose.CST.Located as C
import qualified Language.Goose.CST.Annoted as C
import Language.Goose.LLVM.BuildLibrary (buildAndRun)
import qualified Log.Error as L
import System.Console.Repline
import Control.Monad.State
import qualified Control.Monad.RWS as ST
import Data.List (isPrefixOf)

runParserREPL :: Monad m => String -> m (Either ParseError (Either (Located Expression) (Located Toplevel)))
runParserREPL input = runGoose "REPL" input $ parseEither (parseExpression <* P.eof) (parseToplevel parseExpression)

data REPLState = REPLState {
  ast :: [Located Toplevel],
  libraries :: [String],
  flags :: [String],
  autocompletes :: [String]
} deriving Show

type REPL a = HaskelineT (StateT REPLState IO) a

eval :: String -> REPL ()
eval content = do
  libraries' <- gets libraries
  flags' <- gets flags
  result <- liftIO $ runParserREPL content
  case result of
    Left err -> liftIO $ L.printParseError err "REPL" content
    Right (Right toplevel) -> do
      modify (\s -> s { ast = ast s ++ [toplevel] })
    Right (Left expression) -> case expression of
      z@(Let (C.Annoted name _) _ _ C.:>: _) -> ST.modify $ \s -> s { 
        ast = insertInMain z (ast s),
        autocompletes = name : autocompletes s }
      _ -> do
        ast' <- gets ast
        let ast'' = insertInMain expression ast'
        anf <- liftIO $ getANFDefinitions content ast'' "REPL"
        case anf of
          Nothing -> return ()
          Just anf' -> do
            res <- liftIO $ buildAndRun anf' libraries' flags'
            liftIO $ forM_ res putStrLn

completion :: (Monad m, MonadState REPLState m) => WordCompleter m
completion n = do
  autocompletes' <- gets autocompletes
  return $ filter (isPrefixOf n) autocompletes'

help :: String -> REPL ()
help _ = liftIO $ putStrLn "Commands: :help, :quit, :ast, :libraries, :flags, :addLibrary, :addFlag"

addLibrary :: String -> REPL ()
addLibrary libraries' = do
  let libraries'' = words libraries'
  modify (\s -> s { libraries = libraries'' ++ libraries s })
  liftIO $ putStrLn $ "Added library " ++ unwords libraries''

addFlag :: String -> REPL ()
addFlag flags' = do
  let flags'' = words flags'
  modify (\s -> s { flags = flags'' ++ flags s })
  liftIO $ putStrLn $ "Added flag " ++ unwords flags''

showLibraries :: REPL ()
showLibraries = do
  libraries' <- gets libraries
  if null libraries'
    then liftIO $ putStrLn "No libraries"
    else liftIO $ putStrLn $ "Libraries: " ++ unwords libraries'

showFlags :: REPL ()
showFlags = do
  flags' <- gets flags
  if null flags'
    then liftIO $ putStrLn "No flags"
    else liftIO $ putStrLn $ "Flags: " ++ unwords flags'

options' :: [(String, String -> REPL ())]
options' = [("help", help), ("quit", const abort), ("libraries", const showLibraries), ("flags", const showFlags), ("addLibrary", addLibrary), ("addFlag", addFlag)]

final :: REPL ExitDecision
final = return Exit

init' :: REPL ()
init' = do
  liftIO $ putStrLn "Welcome to Goose REPL"
  liftIO $ putStrLn "Type :help for help"

inputMode :: MultiLine -> REPL String
inputMode SingleLine = return "> "
inputMode MultiLine = return "| "

runREPL :: IO ()
runREPL = do
  let initialState = REPLState [] [] [] reservedWords
  flip evalStateT initialState $ evalRepl inputMode eval options' (Just ':') (Just "multiline") (Word completion) init' final 

insertInMain :: Located Expression -> [Located Toplevel] -> [Located Toplevel]
insertInMain expression (Function (C.Annoted "main" ty) gens args body C.:>: pos : xs) = do
  Function (C.Annoted "main" ty) gens args (Sequence [body, expression] C.:>: pos) C.:>: pos : xs
insertInMain expression (x:xs) = x : insertInMain expression xs
insertInMain expr@(C.Located pos _) [] = [Function (C.Annoted "main" Nothing) [] [] expr C.:>: pos]