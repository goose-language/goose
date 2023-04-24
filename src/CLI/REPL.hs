module CLI.REPL where

import Language.Goose.Parser.Parser
import Language.Goose.CST.Expression (Toplevel (Function), Expression (Sequence, Let))
import Language.Goose.CST.Located (Located)
import Language.Goose.Parser.Modules.Toplevel (parseToplevel)
import Language.Goose.Parser.Lexer (parseEither)
import qualified Text.Parsec as P
import Text.Parsec (ParseError)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode (NoBuffering))
import CLI.Phases (getANFDefinitions)
import qualified Language.Goose.CST.Located as C
import qualified Language.Goose.CST.Annoted as C
import Language.Goose.LLVM.BuildLibrary (buildAndRun)
import Control.Monad (forM_)
import qualified Log.Error as L

runParserREPL :: Monad m => String -> m (Either ParseError (Either (Located Expression) (Located Toplevel)))
runParserREPL input = runGoose "REPL" input $ parseEither (parseExpression <* P.eof) (parseToplevel parseExpression)

runREPL :: [Located Toplevel] -> [String] -> IO ()
runREPL ast libraries = do
  hSetBuffering stdout NoBuffering
  putStr "goose> "
  input <- getLine
  case input of
    ":q" -> return ()
    [] -> runREPL ast libraries
    _ -> do
      result <- runParserREPL input
      case result of
        Left err -> L.printParseError err "REPL" input >> runREPL ast libraries
        Right (Right toplevel) -> runREPL (ast ++ [toplevel]) libraries
        Right (Left expression) -> case expression of
          z@(Let {} C.:>: _) -> runREPL (insertInMain z ast) libraries
          _ -> do
            let ast' = insertInMain expression ast
            anf <- getANFDefinitions input ast' "REPL"
            case anf of
              Nothing -> runREPL ast libraries
              Just anf -> do
                mapM_ print anf
                res <- buildAndRun anf libraries []
                forM_ res putStrLn
                runREPL ast libraries

insertInMain :: Located Expression -> [Located Toplevel] -> [Located Toplevel]
insertInMain expression (Function (C.Annoted "main" ty) gens args body C.:>: pos : xs) = do
  Function (C.Annoted "main" ty) gens args (Sequence [body, expression] C.:>: pos) C.:>: pos : xs
insertInMain expression (x:xs) = x : insertInMain expression xs
insertInMain expr@(C.Located pos _) [] = [Function (C.Annoted "main" Nothing) [] [] expr C.:>: pos]