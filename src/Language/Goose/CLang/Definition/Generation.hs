{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Goose.CLang.Definition.Generation where
import Language.Goose.CLang.Definition.IR
import Language.Goose.CST.Literal
import Language.Goose.CLang.Garbage
import Data.List
import System.Environment
import GHC.IO
import System.FilePath
import Data.List.Utils
import Control.Monad.State

type MonadGeneration m = MonadState Int m

rttiName :: String
rttiName = "VALUE"

class Generation a where
  generate :: MonadGeneration m => a -> m String

reservedWords :: [String]
reservedWords = ["auto", "else", "long", "switch", "break", "enum", "register", "typedef", "case", "extern", "return", "union", "char", "float", "short", "unsigned", "const", "for", "signed", "void", "continue", "goto", "sizeof", "volatile", "default", "if", "static", "while", "do", "int", "struct", "_Packed", "double"]

varify :: String -> String
varify n = if n `elem` reservedWords then n' ++ "_" else n'
  where n' = replace "::" "_" n

instance Generation IRToplevel where
  generate (IRFunction name args body) = do
    body' <- generate body
    case name of
      "main" -> return $ "int main(int argc, char **argv) { " ++ startGarbage body' ++ "}"
      "$$init$$" -> do
        decls <- mapM (\case
          IRUpdate name' e -> do
            e' <- generate e
            name'' <- generate name'
            return $ name'' ++ " = " ++ e' ++ ";"
          _ -> error "Should not encounter other statements than updates.") body
        return $  "void $$init$$() {" ++ concat decls ++ "}"
      _ ->  return $ rttiName ++ " " ++ varify name ++ "(" ++ (if null args then "" else rttiName ++ " args") ++ ") { " ++ unlines (zipWith (\arg i -> rttiName ++ " " ++ varify arg ++ " = " ++ "index_(args, " ++ show (i :: Integer) ++ ");") args [0..]) ++ body'  ++ "}"
  generate (IRDeclaration name e) = do
    e' <- generate e
    return $ rttiName ++ " " ++ varify name ++ " = " ++ e' ++ ";"
  generate (IRExtern name _) = return $ "extern " ++ rttiName ++ "* " ++ name ++ ";"
  generate (IRStruct name fields) = do
    fields' <- mapM generate fields
    return $ "struct " ++ varify name ++ " { " ++ unwords fields' ++ " };"
  generate (IRDeclare name args ret) = case args of
    [] -> return $ ret ++ " " ++ varify name ++ ";"
    _ -> return $ ret ++ " " ++ varify name ++ "(" ++ intercalate ", " args ++ ");"
  generate (IRGlobalString name str) = return $ "const char*" ++ varify name ++ " = " ++ show str ++ ";"

instance Generation IRStructField where
  generate (IRStructField name ty) = return $ ty ++ " " ++ varify name ++ ";"
  generate (IRStructUnion name fields) = do
    fields' <- mapM generate fields
    return $ "union " ++ varify name ++ " { " ++ unwords fields' ++ " };"
  generate (IRStructStruct name fields) = do
    fields' <- mapM generate fields
    return $ "struct " ++ varify name ++ " { " ++ unwords fields' ++ " };"

fresh :: MonadGeneration m => m String
fresh = do
  i <- get
  put (i + 1)
  return $ "$$" ++ show i

instance Generation IRStatement where
  generate (IRReturn e) = do
    e' <- generate e
    return $ "return " ++ e' ++ ";"
  generate (IRIfElse e t f) = do
    e' <- generate e
    t' <- generate t
    f' <- generate f
    return $ "if (decode_boolean(" ++ e' ++ ")) {" ++ t' ++ "} else {" ++ f' ++ "}"
  generate (IRIf e t) = do
    e' <- generate e
    t' <- generate t
    return $ "if (decode_boolean(" ++ e' ++ ")) {" ++ t' ++ "}"
  generate (IRWhile e s) = do
    e' <- generate e
    s' <- generate s
    return $ "while (decode_boolean(" ++ e' ++ ")) {" ++ s' ++ "}"
  generate (IRFor name e s) = do
    let var = varify name
    acc <- (("$$__acc__" ++ var)++) <$> fresh
    e' <- generate e
    s' <- generate s
    return $ "Array " ++ acc ++ " = decode_pointer(" ++ e' ++ ")->as_array;" ++ "for (int $$i = 0; $$i < " ++ acc ++ ".length; $$i++) { " ++ rttiName ++ " " ++ varify name ++ " = " ++ acc ++ ".data[$$i]; " ++  s' ++ "}"
  generate (IRExpression e) = (++";") <$> generate e
  generate (IRBlock s) = do
    s' <- generate s
    return $ "{" ++ s' ++ "}"
  generate IRBreak = return "break;"
  generate IRContinue = return "continue;"
  generate (IRDeclarationStatement name e@(IRDictAccess _ "$$fun")) = do
    e' <- generate e
    return $ rttiName ++ " (*" ++ varify name ++ ")(" ++ rttiName ++ ") = " ++ e' ++ ";"
  generate (IRDeclarationStatement name e) = do
    e' <- generate e
    return $ rttiName ++ " " ++ varify name ++ " = " ++ e' ++ ";"
  generate (IRUpdate e1 e2) = do
    e1' <- generate e1
    e2' <- generate e2
    return $ "update(&" ++ e1' ++ ", " ++ e2' ++ ");"

generateString :: String -> String
generateString [] = "emptyList()"
generateString xs = "string(" ++ show xs ++ ")"

generateList :: MonadGeneration m => [IRExpression] -> m String
generateList [] = return "emptyList()"
generateList xs = do
  xs' <- mapM generate xs
  return $ "list(" ++ show (length xs) ++ ", " ++ intercalate ", " xs'  ++ ")"

instance Generation IRExpression where
  generate (IRVariable name) = return $ varify name
  generate (IRApplication e args) = do
    e' <- generate e
    args' <- mapM generate args
    return $ e' ++ "(" ++ intercalate ", " args' ++ ")"
  generate (IRBinary "&&" e1 e2) = do
    e1' <- generate e1
    e2' <- generate e2
    return $ "boolean(decode_boolean(" ++ e1' ++ ") && decode_boolean(" ++ e2' ++ "))"
  generate (IRBinary "||" e1 e2) = do
    e1' <- generate e1
    e2' <- generate e2
    return $ "boolean(decode_boolean(" ++ e1' ++ ") || decode_boolean(" ++ e2' ++ "))"
  generate (IRBinary op e1 e2) = do
    e1' <- generate e1
    e2' <- generate e2
    return $ "(" ++ e1' ++ " " ++ op ++ " " ++ e2' ++ ")"
  generate (IRUnary op e) = do
    e' <- generate e
    return $ "(" ++ op ++ e' ++ ")"
  generate (IRLiteral l) = case l of
    Int i -> return $ "integer(" ++ show i ++ ")"
    Float f -> return $ "floating(" ++ show f ++ ")"
    Bool True -> return "boolean(1)"
    Bool False -> return "boolean(0)"
    Char c -> return $ "character(" ++ show c ++ ")"
    String s -> return $ generateString s
    Unit -> return "unit()"
  generate (IRList es) = generateList es
  generate (IRListAccess e1 e2) = do
    e1' <- generate e1
    e2' <- generate e2
    return $ "index_(" ++ e1' ++ ", decode_integer(" ++ e2' ++ "))"
  generate (IRTernary e1 e2 e3) = do
    e1' <- generate e1
    e2' <- generate e2
    e3' <- generate e3
    return $ "decode_boolean(" ++ e1' ++ ") ? " ++ e2' ++ " : " ++ e3'
  generate (IREUpdate e1 e2) = do
    e1' <- generate e1
    e2' <- generate e2
    return $ "update(" ++ e1' ++ ", " ++ e2' ++ ");"
  generate (IRDict es) = generateStruct es
  generate (IRDictAccess e1 "$$fun") = do
    e1' <- generate e1
    return $ "decode_lambda(" ++ e1' ++ ")"
  generate (IRDictAccess e1 e2) = do
    e1' <- generate e1
    return $ "property_(" ++ e1' ++ ", " ++ show e2 ++ ")"
  generate (IRReference e) = ("&" ++) <$> generate e
  generate (IRIn e1 s) = do
    e1' <- generate e1
    s' <- generate (IRLiteral (String s))
    return $ "Array_has(list(2, " ++ e1' ++ ", " ++ s' ++ "))"

generateStruct :: MonadGeneration m => [(String, IRExpression)] -> m String
generateStruct [] = return "NULL"
generateStruct xs = do
  xs' <- mapM go xs
  return $ "structure(" ++ show (length xs) ++ ", " ++ intercalate ", " xs' ++ " )"
  where go (x, e) = do
          e' <- generate e
          return $  show x ++ ", " ++ e'

instance Generation a => Generation [a] where
  generate = (unlines <$>) . mapM generate

getGoosePath :: String
getGoosePath = unsafeDupablePerformIO $
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
  let rttiError = "std/core/error.c"
  let garbageTGC = "std/core/garbage/tgc.c"
  let garbage = "std/core/garbage.c"
  let http = "std/core/http.c"

  map (getGoosePath </>) [rttiMaker, rttiNum, rttiEq, rttiList, rttiIO, rttiConv,
                          rttiRegex, rttiType, rttiError, garbageTGC, garbage, http]

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
  let garbageTGC = "std/core/garbage/tgc.h"
  let garbage = "std/core/garbage.h"
  let nanBox = "std/core/value/nanbox.h"
  let http = "std/core/http.h"

  map (getGoosePath </>) [rttiHeader, rttiNum, rttiEq, rttiList, rttiIO, rttiConv, rttiRegex, nanBox, rttiType, garbageTGC, garbage, http]

generateHeaders :: [String] -> String
generateHeaders = unlines . map (\x -> "#include \"" ++ x ++ "\"")

getAllFunctions :: [IRToplevel] -> [String]
getAllFunctions = nub . concatMap go
  where go :: IRToplevel -> [String]
        go (IRFunction name _ _) = [name]
        go _ = []