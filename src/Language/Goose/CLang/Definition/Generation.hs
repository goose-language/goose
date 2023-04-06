{-# LANGUAGE LambdaCase #-}
module Language.Goose.CLang.Definition.Generation where
import Language.Goose.CLang.Definition.IR
import Language.Goose.CST.Literal
import Data.List
import System.Environment
import GHC.IO
import System.FilePath

rttiName :: String
rttiName = "Value"

class Generation a where
  generate :: a -> String

reservedWords :: [String]
reservedWords = ["auto", "else", "long", "switch", "break", "enum", "register", "typedef", "case", "extern", "return", "union", "char", "float", "short", "unsigned", "const", "for", "signed", "void", "continue", "goto", "sizeof", "volatile", "default", "if", "static", "while", "do", "int", "struct", "_Packed", "double"]

varify :: String -> String
varify n = if n `elem` reservedWords then n ++ "_" else n

instance Generation IRToplevel where
  generate (IRFunction name args body) = 
    (if name == "main" then "int " else rttiName ++ "* ") ++ (varify name) ++ "(" ++ (if null args then "" else rttiName ++ "* args") ++ ") { " ++ unlines (zipWith (\arg i -> rttiName ++ "* " ++ arg ++ " = " ++ "index_(args, " ++ show (i :: Integer) ++ ");") args [0..]) ++ unlines (map generate body)  ++ "}"
  generate (IRDeclaration name e) = rttiName ++ "* " ++ (varify name) ++ " = " ++ generate e ++ ";"
  generate (IRExtern name _) = "extern " ++ rttiName ++ "* " ++ name ++ ";"
  generate (IRStruct name fields) = "struct " ++ (varify name) ++ " { " ++ unwords (map generate fields) ++ " };"
  -- generate (IRDeclare name args ret) = case args of
  --   [] -> ret ++ " " ++ name ++ ";"
  --   _ -> ret ++ " " ++ name ++ "(" ++ intercalate ", " args ++ ");"
  generate _ = ""

instance Generation IRStructField where
  generate (IRStructField name ty) = ty ++ " " ++ (varify name) ++ ";"
  generate (IRStructUnion name fields) = "union " ++ (varify name) ++ " { " ++ unwords (map generate fields) ++ " };"
  generate (IRStructStruct name fields) = "struct " ++ (varify name) ++ " { " ++ unwords (map generate fields) ++ " };"

instance Generation IRStatement where
  generate (IRReturn e) = "return " ++ generate e ++ ";"
  generate (IRIf e t f) = "if (" ++ generate e ++ "->b ) {" ++ unlines (map generate t) ++ "} else {" ++ unlines (map generate f) ++ "}"
  generate (IRWhile e s) = "while (" ++ generate e ++ "->b) {" ++ unlines (map generate s) ++ "}"
  generate (IRFor name e s) = do
    "for (Value* v = " ++ generate e ++ "; v != NULL && v->l.value != NULL; v = v->l.next) { Value* " ++ (varify name) ++ " = v->l.value; " ++  unlines (map generate s) ++ "}"
  generate (IRExpression e) = generate e ++ ";"
  generate (IRBlock s) = "{" ++ unlines (map generate s) ++ "}"
  generate IRBreak = "break;"
  generate IRContinue = "continue;"
  generate (IRDeclarationStatement name e@(IRDictAccess _ "$$fun")) = rttiName ++ "* (*" ++ (varify name) ++ ")(Value*) = " ++ generate e ++ ";"
  generate (IRDeclarationStatement name e) = rttiName ++ "* " ++ (varify name) ++ " = " ++ generate e ++ ";"
  generate (IRUpdate e1 e2) = "update(" ++ generate e1 ++ ", " ++ generate e2 ++ ");"

generateString :: String -> String
generateString [] = "emptyList()"
generateString xs = "string(" ++ show xs ++ ")"

generateList :: [IRExpression] -> String
generateList [] = "emptyList()"
generateList xs = go xs
  where go :: [IRExpression] -> String
        go [] = "NULL"
        go (x:xs') = "list(" ++ generate x ++ ", " ++ go xs' ++ ")"

instance Generation IRExpression where
  generate (IRVariable name) = varify name
  generate (IRApplication e args) = generate e ++ "(" ++ intercalate "," (map generate args) ++ ")"
  generate (IRBinary op e1 e2) = "(" ++ generate e1 ++ " " ++ op ++ " " ++ generate e2 ++ ")"
  generate (IRUnary op e) = "(" ++ op ++ " " ++ generate e ++ ")"
  generate (IRLiteral l) = case l of
    Int i -> "integer(" ++ show i ++ ")"
    Float f -> "floating(" ++ show f ++ ")"
    Bool True -> "boolean(1)"
    Bool False -> "boolean(0)"
    Char c -> "character(" ++ show c ++ ")"
    String s -> generateString s
    Unit -> "unit()"
  generate (IRList es) = generateList es
  generate (IRListAccess e1 e2) = "index_(" ++ generate e1 ++ ", " ++ generate e2 ++ "->i)"
  generate (IRTernary e1 e2 e3) = "(" ++ generate e1 ++ "->b ? " ++ generate e2 ++ " : " ++ generate e3 ++ ")"
  generate (IREUpdate e1 e2) = "update(" ++ generate e1 ++ ", " ++ generate e2 ++ ")"
  generate (IRDict es) = generateStruct es
  generate (IRDictAccess e1 "$$fun") = generate e1 ++ "->$$fun"
  generate (IRDictAccess e1 e2) = "property_(" ++ generate e1 ++ ", " ++ show e2 ++ ")"
  generate (IRReference e) = "&" ++ generate e

generateStruct :: [(String, IRExpression)] -> String
generateStruct [] = "NULL"
generateStruct ((name, e):xs) = "structure(" ++ show name ++ ", " ++ generate e ++ ", " ++ generateStruct xs ++ ")"

instance Generation a => Generation [a] where
  generate = unlines . map generate

getGoosePath :: String
getGoosePath = unsafeDupablePerformIO $ do
  lookupEnv "GOOSE" >>= \case
    Just path -> return path
    Nothing -> error "GOOSE environment variable not set."

includeLibrary :: [String]
includeLibrary = do
  let rttiMaker = "std/core/value.c"
  let rttiNum = "std/core/num.c"
  let rttiEq = "std/core/eq.c"
  let rttiList = "std/core/list.c"
  let rttiIO = "std/core/io.c"
  let rttiConv = "std/core/conversion.c"
  let rttiRegex = "std/core/regex.c"
  let rttiType = "std/core/type.c"
  
  map (getGoosePath </>) [rttiMaker, rttiNum, rttiEq, rttiList, rttiIO, rttiConv, rttiRegex, rttiType]

includeHeaders :: [String]
includeHeaders = do
  let rttiHeader = "std/core/value.h"
  let rttiNum = "std/core/num.h"
  let rttiEq = "std/core/eq.h"
  let rttiList = "std/core/list.h"
  let rttiIO = "std/core/io.h"
  let rttiConv = "std/core/conversion.h"
  let rttiRegex = "std/core/regex.h"
  let rttiType = "std/core/type.h"

  map (getGoosePath </>) [rttiHeader, rttiNum, rttiEq, rttiList, rttiIO, rttiConv, rttiRegex, rttiType]

generateHeaders :: [String] -> String
generateHeaders = unlines . map (\x -> "#include \"" ++ x ++ "\"")

getAllFunctions :: [IRToplevel] -> [String]
getAllFunctions = nub . concatMap go
  where go :: IRToplevel -> [String]
        go (IRFunction name _ _) = [name]
        go _ = []