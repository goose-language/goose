module Language.Goose.Transformation.DeclarationRemover where

import Language.Goose.Transformation.ANF.AST
import Language.Goose.Transformation.ANF.ANF
import qualified Language.Goose.CST.Annoted as C

getAllDeclarations :: [ANFDefinition] -> [(String, ANFExpression)]
getAllDeclarations = go
  where go :: [ANFDefinition] -> [(String, ANFExpression)]
        go [] = []
        go (DDeclaration name expr : defs) = (name, expr) : go defs
        go (_ : defs) = go defs

removeDeclarations :: [ANFDefinition] -> [ANFDefinition]
removeDeclarations = go
  where go :: [ANFDefinition] -> [ANFDefinition]
        go [] = []
        go (DDeclaration name _ : defs) = DDeclare (name C.:@ v) : go defs
        go (def : defs) = def : go defs

addInitCall :: [ANFStatement] -> [ANFDefinition] -> [ANFDefinition]
addInitCall defs = map go
  where go :: ANFDefinition -> ANFDefinition
        go (DFunction "main" args body) = DFunction "main" args (defs ++ body)
        go def = def

addInitFunction :: [ANFDefinition] -> [ANFDefinition]
addInitFunction defs =  toplevels
  where defs_ = getAllDeclarations defs
        toplevels = addInitCall (map (\(name, expr) -> SUpdate (EVariable name v) expr) defs_) $ removeDeclarations defs

