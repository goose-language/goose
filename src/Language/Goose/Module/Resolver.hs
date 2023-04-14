{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Goose.Module.Resolver where
import Language.Goose.CST.Located
import Language.Goose.CST.Expression
import Control.Monad.IO.Class
import Control.Monad.Except ( MonadError(throwError), runExceptT )
import Language.Goose.Parser.Parser
import System.Exit
import Log.Error
import System.Directory
import System.FilePath
import Language.Goose.CLang.Definition.Generation

type MonadResolver m = (MonadIO m, MonadError (String, Position) m)

resolveImport :: MonadResolver m => String -> String -> Position -> m [Located Toplevel]
resolveImport _ name pos = do
  let stdPath = getGoosePath
  let stdName = stdPath </> name
  let path = if startsWith "std" name then stdName else name

  fileExists <- liftIO $ doesFileExist path
  if not fileExists
    then throwError ("Could not find module " ++ name, pos)
    else return ()

  content <- liftIO $ readFile path
  ast <- parseGoose name content
  case ast of
    Left err -> liftIO $ printParseError err name content >> exitFailure
    Right ast' -> resolveImports (takeDirectory path) ast'

startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (x:xs) (y:ys) = x == y && startsWith xs ys

-- | Resolve the imports of a module.
resolveImports :: MonadResolver m => String -> [Located Toplevel] -> m [Located Toplevel]
resolveImports dir (Located pos (Import name) : toplevels) = do
  resolved <- resolveImport dir name pos
  rest <- resolveImports dir toplevels
  return $ (Namespace "_" resolved :>: pos) : rest 
resolveImports dir (Located pos (ImportAs name alias) : toplevels) = do
  resolved <- resolveImport dir name pos
  rest <- resolveImports dir toplevels
  return $ (Namespace ("$" ++ alias) resolved :>: pos) : rest 
resolveImports dir (toplevel : toplevels) = do
  rest <- resolveImports dir toplevels
  return $ toplevel : rest
resolveImports _ [] = return []

runModuleResolver :: MonadIO m => String -> [Located Toplevel] -> m (Either (String, Position) [Located Toplevel])
runModuleResolver dir toplevels = runExceptT (resolveImports (takeDirectory dir) toplevels)