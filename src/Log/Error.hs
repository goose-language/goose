{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Log.Error where
  import Error.Diagnose.Compat.Parsec ( errorDiagnosticFromParseError, HasHints(..) )
  import Error.Diagnose               ( addFile, printDiagnostic, stderr, defaultStyle, Position (Position), Marker (This), def, stdout, addReport, Report(Err), Note (Note) )
  import Data.Void                    ( Void )
  import Text.Parsec                  ( SourcePos, sourceColumn, sourceLine, sourceName, ParseError )
  import Data.Maybe                   ( maybeToList, fromJust )
  import System.Directory (doesFileExist)
  
  instance HasHints Void String where
    hints _ = mempty

  printError :: Maybe String -> (String, Maybe String, (SourcePos, SourcePos)) -> String -> IO ()
  printError content (error', msg, (p1, p2)) step = do
    let p1' = (sourceLine p1, sourceColumn p1)
    let p2' = (sourceLine p2, sourceColumn p2)
    let file' = sourceName p1
    b <- doesFileExist file'
    x' <- if b then readFile file' else return $ fromJust content
    let pos' = Position p1' p2' $ sourceName p1
    let beautifulExample = Err
          Nothing
          error'
          [ (pos', This step) ]
          (map Note $ maybeToList msg)

    -- Create the diagnostic 
    let diagnostic  = addFile def file' x'
    let diagnostic' = addReport diagnostic beautifulExample

    -- Print with unicode characters, colors and the default style
    printDiagnostic stdout True True 4 defaultStyle diagnostic'

  printParseError :: ParseError -> String -> String -> IO ()
  printParseError err' file x = do
    let diag  = errorDiagnosticFromParseError Nothing "Parse error on input" Nothing err'
        diag' = addFile diag file x
      in printDiagnostic stderr True True 4 defaultStyle diag'