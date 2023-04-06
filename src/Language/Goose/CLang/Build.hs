{-# LANGUAGE LambdaCase #-}
module Language.Goose.CLang.Build where
import System.Process
import System.Directory
import System.FilePath

import Language.Goose.CLang.Compiler
import Language.Goose.Transformation.ANF.AST
import Language.Goose.CLang.Definition.Generation

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
generateDefinition name = "Value* " ++ name ++ "(Value* args);"

build :: [ANFDefinition] -> String -> [String] -> [String] -> IO ()
build ast file libraries headers = do
  let output = file -<.> ".c"
  let ast' = compile ast
  let funs = getAllFunctions ast' \\ ["main"]
  let decls = unlines $ map generateDefinition funs
  
  writeFile output $ unlines [generateHeaders headers, "#include <stdlib.h>", decls, generate ast']
  compiler <- getCLangCompiler
  callProcess compiler $ libraries ++ [output] ++ ["-w", "-g"]
  putStrLn $ compiler ++ " " ++ unwords (libraries ++ [output] ++ ["-w", "-g"])
  