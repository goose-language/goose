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
import Language.Goose.Transformation.AlphaConversion
import Language.Goose.LLVM.BuildLibrary (buildExecutable)
import Data.List

import qualified Log.Error as L
import Data.Functor
import Language.Goose.Transformation.ANF.AST (ANFDefinition)
import Language.Goose.CST.Located (Located)
import Language.Goose.CST.Expression (Toplevel)

compileFromString :: String -> FilePath -> [String] -> [String] -> String -> String -> IO ()
compileFromString input filename libraries flags output target = do
  ast <- parseGoose filename input
  case ast of
    Left err -> L.printParseError err filename input
    Right ast' -> do
      ast <- runModuleResolver filename ast'
      case ast of
        Left err -> L.printError Nothing (fst err, Nothing, snd err) "Module resolution"
        Right ast' -> do
          ast <- runModuleBundling $ nub ast'
          case ast of
            Left err -> L.printError Nothing (fst err, Nothing, snd err) "Module bundling"
            Right ast' -> do
              ast <- performInfer $ nub ast'
              case ast of
                Left err -> L.printError Nothing err "Type inference"
                Right ast' -> do
                  ast <- return $ runEtaExpansion ast'
                  ast <- runANF ast
                  ast <- return $ runClosureConversion ast
                  ast <- return $ addInitFunction ast
                  ast <- return $ runAlphaConversion ast
                  buildExecutable ast libraries flags output target

getANFDefinitions :: String -> [Located Toplevel] -> String -> IO (Maybe [ANFDefinition])
getANFDefinitions content ast' filename = do
  ast <- runModuleResolver filename ast'
  case ast of
    Left err -> L.printError (Just content) (fst err, Nothing, snd err) "Module resolution" $> Nothing
    Right ast' -> do
      ast <- runModuleBundling $ nub ast'
      case ast of
        Left err -> L.printError (Just content) (fst err, Nothing, snd err) "Module bundling" $> Nothing
        Right ast' -> do
          ast <- performInfer $ nub ast'
          case ast of
            Left err -> L.printError (Just content) err "Type inference" $> Nothing
            Right ast' -> do
              ast <- return $ runEtaExpansion ast'
              ast <- runANF ast
              ast <- return $ runClosureConversion ast
              return . Just $ addInitFunction ast
                