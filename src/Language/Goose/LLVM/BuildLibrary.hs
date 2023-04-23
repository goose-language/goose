{-# LANGUAGE LambdaCase #-}
module Language.Goose.LLVM.BuildLibrary where
import qualified Language.Goose.CLang.Definition.Generation as CL
import qualified System.Directory as IO
import qualified System.Process as IO
import Language.Goose.Transformation.ANF.AST (ANFDefinition)
import qualified Language.Goose.LLVM.Compiler as LLVM
import Language.Goose.CLang.Compiler (compile)
import System.FilePath

buildLibrary :: [String] -> IO (Maybe String)
buildLibrary libs = do
  let libraries = CL.includeLibrary
  IO.findExecutable "clang" >>= \case
    Just command -> IO.callProcess command ("-c" : "-w" : libraries ++ libs) >> return (Just command)
    Nothing -> putStrLn "No CLang compiler LLVM found!" >> return Nothing

buildExecutable :: [ANFDefinition] -> [String] -> [String] -> String -> IO ()
buildExecutable ast libs flags output = do
  clang <- buildLibrary libs
  x <- LLVM.runCompiler (compile ast)
  writeFile "main.ll" x

  case clang of
    Just command -> do
      IO.callProcess "llc" ["main.ll", "-filetype=obj", "-o", "main.o"]
      IO.callCommand . unwords $ [command, "*.o", "-o", output, "-w"] ++ map ("-l"++) flags
      IO.removeFile "main.o"
      mapM_ (IO.removeFile . (-<.> "o") . takeFileName) (CL.includeLibrary ++ libs)
    Nothing -> return ()