module Language.Goose.Transformation.DeclarationRemover where

import Language.Goose.Transformation.ANF.AST
import Language.Goose.Transformation.ANF.ANF

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
        go (DDeclaration name _ : defs) = DInternDeclare name : go defs
        go (def : defs) = def : go defs

addInitCall :: [ANFDefinition] -> [ANFDefinition]
addInitCall defs = map go defs
  where go :: ANFDefinition -> ANFDefinition
        go (DFunction "main" args body) = DFunction "main" args ((SExpression $ EApplication (EVariable "$$init$$" v) []) : body)
        go def = def

addInitFunction :: [ANFDefinition] -> [ANFDefinition]
addInitFunction defs =  toplevels ++ [initFunction]
  where defs_ = getAllDeclarations defs
        toplevels = addInitCall $ removeDeclarations defs
        initFunction = DFunction "$$init$$" [] (map (\(name, expr) -> SUpdate (EVariable name v) expr) defs_)

