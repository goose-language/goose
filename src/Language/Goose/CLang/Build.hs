{-# LANGUAGE LambdaCase #-}
module Language.Goose.CLang.Build where
import System.Process
import System.Directory
import System.FilePath

import Language.Goose.CLang.Compiler
import Language.Goose.Transformation.ANF.AST
import Language.Goose.CLang.Definition.Generation

getCLangCompiler :: IO String
getCLangCompiler = do
  let clang = "clang"
  let gcc = "gcc"
  findExecutable clang >>= \case
    Just _ -> return clang
    Nothing -> findExecutable gcc >>= \case
      Just _ -> return gcc
      Nothing -> error "No C compiler found"

build :: [ANFDefinition] -> String -> [String] -> [String] -> IO ()
build ast file libraries headers = do
  let output = file -<.> ".c"
  writeFile output $ unlines [generateHeaders headers, "#include <stdlib.h>", generate (compile ast)]
  compiler <- getCLangCompiler
  callProcess compiler $ libraries ++ [output] ++ [ "-g"]
  