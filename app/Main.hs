{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
module Main where
import Language.Goose.Parser.Parser
import Language.Goose.Module.Resolver
import Language.Goose.Module.Bundler
import Language.Goose.Typecheck.Checker
import Language.Goose.Transformation.ANF.ANF
import Language.Goose.Transformation.Closure.Conversion
import Language.Goose.Transformation.Closure.Hoisting
import Language.Goose.Transformation.EtaExpansion
import Language.Goose.CLang.Build
import Language.Goose.CLang.Definition.Generation
import Language.Goose.Transformation.Garbage.Collector
import System.Environment
import Data.List

import qualified Log.Error as L

chunkBy2 :: [a] -> [(a, a)]
chunkBy2 [] = []
chunkBy2 (x:y:xs) = (x, y) : chunkBy2 xs
chunkBy2 _ = error "chunkBy2: odd number of elements"

main :: IO ()
main = do
  getArgs >>= \case
    file:libraries -> do
      content <- readFile file
      ast <- parseGoose file content
      case ast of
        Left err -> L.printParseError err file content
        Right ast' -> do
          ast <- runModuleResolver file ast'
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
                      ast <- return $ runHoisting ast
                      ast <- return $ runCollector ast
                      let (libraries', headers) = unzip $ chunkBy2 libraries
                      build ast "main" (libraries' ++ includeLibrary) (includeHeaders ++ headers)
    _ -> putStrLn "Usage: goose <file> [libraries]"