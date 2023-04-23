{-# OPTIONS_GHC -Wno-name-shadowing #-}
module CLI.Phases where
import Language.Goose.Parser.Parser
import Language.Goose.Module.Resolver
import Language.Goose.Module.Bundler
import Language.Goose.Typecheck.Checker
import Language.Goose.Transformation.ANF.ANF
import Language.Goose.Transformation.Closure.Conversion
import Language.Goose.Transformation.EtaExpansion
import Language.Goose.Transformation.DeclarationRemover
import Language.Goose.LLVM.BuildLibrary (buildExecutable)
import Data.List

import qualified Log.Error as L

compileFromString :: String -> FilePath -> [String] -> [String] -> String -> IO ()
compileFromString input filename libraries flags output = do
  ast <- parseGoose filename input
  case ast of
    Left err -> L.printParseError err filename input
    Right ast' -> do
      ast <- runModuleResolver filename ast'
      case ast of
        Left err -> L.printError (fst err, Nothing, snd err) "Module resolution"
        Right ast' -> do
          ast <- runModuleBundling $ nub ast'
          case ast of
            Left err -> L.printError (fst err, Nothing, snd err) "Module bundling"
            Right ast' -> do
              ast <- performInfer $ nub ast'
              case ast of
                Left err -> L.printError err "Type inference"
                Right ast' -> do
                  ast <- return $ runEtaExpansion ast'
                  ast <- runANF ast
                  ast <- return $ runClosureConversion ast
                  ast <- return $ addInitFunction ast
                  buildExecutable ast libraries flags output