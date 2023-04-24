{-# LANGUAGE LambdaCase #-}
module Language.Goose.LLVM.BuildLibrary where
import qualified Language.Goose.CLang.Definition.Generation as CL
import qualified System.Directory as IO
import qualified System.Process as IO
import Language.Goose.Transformation.ANF.AST (ANFDefinition)
import qualified Language.Goose.LLVM.Compiler as LLVM
import Language.Goose.CLang.Compiler (compile)
import System.FilePath
import System.IO.Temp (withTempDirectory)
import Language.Goose.CLang.Definition.Generation (getGoosePath)
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import qualified GHC.IO.Exception as IO
import Data.Functor

buildExecutable :: [ANFDefinition] -> [String] -> [String] -> String -> IO ()
buildExecutable ast libs flags output = do
  let libraries = CL.includeLibrary
  IO.findExecutable "clang" >>= \case
    Just command -> do
      current <- getCurrentDirectory
      withTempDirectory current "build" $ \dir -> (do
        setCurrentDirectory dir
        IO.callProcess command $ libraries ++ map (current </>) libs ++ ["-c", "-w", "-lcurl"] ++ map ("-l"++) flags
        x <- LLVM.runCompiler (compile ast)
        writeFile (dir </> "main.ll") x
        IO.callProcess "llc" [dir </> "main.ll", "-filetype=obj", "-o", dir </> "main.o"]
        setCurrentDirectory current
        IO.callCommand . unwords $ [command, dir </> "*.o", "-o", output, "-w", "-lcurl"] ++ map ("-l"++) flags)
    Nothing -> putStrLn "No CLang compiler LLVM found!"

buildAndRun :: [ANFDefinition] -> [String] -> [String] -> IO (Maybe String)
buildAndRun ast libs flags = do
  let libraries = CL.includeLibrary
  IO.findExecutable "clang" >>= \case
    Just command -> do
      withTempDirectory getGoosePath "build" $ \dir -> (do
        current <- getCurrentDirectory
        setCurrentDirectory dir
        IO.callProcess command $ libraries ++ libs ++ ["-c", "-w", "-lcurl"] ++ map ("-l"++) flags
        x <- LLVM.runCompiler (compile ast)
        writeFile (dir </> "main.ll") x
        IO.callProcess "llc" [dir </> "main.ll", "-filetype=obj", "-o", dir </> "main.o"]
        setCurrentDirectory current
        IO.callCommand . unwords $ [command, dir </> "*.o", "-o", dir </> "tempexe", "-w", "-lcurl"] ++ map ("-l"++) flags
        (exitCode, stdout, stderr) <- IO.readCreateProcessWithExitCode (IO.proc (dir </> "tempexe") []) ""
        case exitCode of
          IO.ExitSuccess -> return (Just stdout)
          IO.ExitFailure _ -> putStrLn stderr $> Nothing)
    Nothing -> putStrLn "No CLang compiler LLVM found!" $> Nothing
