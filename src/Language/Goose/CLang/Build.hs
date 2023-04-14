{-# LANGUAGE LambdaCase #-}
module Language.Goose.CLang.Build where
import System.Process
import System.Directory
import System.FilePath

import Language.Goose.CLang.Compiler
import Language.Goose.Transformation.ANF.AST
import Language.Goose.CLang.Definition.Generation
import Control.Monad.State

import Data.List

getCLangCompiler :: IO String
getCLangCompiler = do
  let clang = "clang"
  let gcc = "gcc"
  findExecutable clang >>= \case
    Just _ -> return clang
    Nothing -> findExecutable gcc >>= \case
      Just _ -> return gcc
      Nothing -> error "No C compiler found"

generateDefinition :: String -> String
generateDefinition "$$init$$" = "void $$init$$();"
generateDefinition name = rttiName ++ " " ++ varify name ++ "(" ++ rttiName ++ " args);"

build :: [ANFDefinition] -> String -> [String] -> [String] -> IO ()
build ast file libraries headers = do
  let output = file -<.> ".c"
  let ast' = compile ast
  let funs = getAllFunctions ast' \\ ["main"]
  let decls = unlines $ map generateDefinition funs
  
  let cOutput = evalState (generate ast') 0

  writeFile output $ unlines [generateHeaders headers, "#include <stdlib.h>", decls, cOutput]
  compiler <- getCLangCompiler
  callProcess compiler $ libraries ++ [output] ++ ["-w", "-g"]
  -- putStrLn $ compiler ++ " " ++ unwords (libraries ++ [output] ++ ["-w", "-g"])
  