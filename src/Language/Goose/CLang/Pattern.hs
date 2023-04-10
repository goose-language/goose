{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Language.Goose.CLang.Pattern where
import Language.Goose.CST.Literal ( Literal(String, Int) )
import Language.Goose.CLang.Definition.IR
import Prelude hiding ( and )
import Data.Maybe
import Language.Goose.Typecheck.Definition.AST

and :: IRExpression -> IRExpression -> IRExpression
and = IRBinary "&&"

findPattern :: Pattern -> (IRExpression -> [(Maybe IRStatement, Maybe IRExpression)])
findPattern (PLiteral l)    =
  \e -> [(Nothing, Just $ IRApplication (IRVariable "eq") [e, IRLiteral l])]
findPattern PWildcard   =
  const [(Nothing, Nothing)]
findPattern (PVariable v _)  = do
  \e -> [(Just $ IRDeclarationStatement v e, Nothing)]
findPattern (PConstructor n xs) = do
  \e -> do
    let xs' = zipWith (\x y -> findPattern y (IRDictAccess e x)) (["a" ++ show i | i <- [0..]]) xs
    concat ([(Nothing, Just $ IRApplication (IRVariable "eq") [IRDictAccess e "type", IRLiteral (String n)])] : xs')
findPattern (PList pats) = do
  let pats' = map findPattern pats
  \x -> do
    let pats'' = zipWith (\i f -> f (IRListAccess x (IRLiteral $ Int i))) [0..] pats'
    concat pats'' ++ [(Nothing, Just $ IRApplication (IRVariable "eq") [IRApplication (IRVariable "Array_length") [IRList [x]], (IRLiteral $ Int $ toInteger $ length pats)])]
findPattern (PStructure fields) = do
  let args' = map (\(x, y) -> (x, findPattern y)) fields
  \e -> do
      let args'' = concatMap (\(x, f) -> f (IRDictAccess e x)) args'
      concatMap (\(x, _) -> [(Nothing, Just $ IRIn e x)]) fields ++ args''

compileCase :: Pattern -> IRExpression -> [IRStatement] -> IRStatement
compileCase (PVariable n _) = do
  \x b -> 
    IRBlock $ IRDeclarationStatement n x : b
compileCase (PConstructor n args) = do
  let args' x = concat $ zipWith (\arg v -> do
            let f = findPattern arg
            f (IRDictAccess x v)) args (["a" ++ show i | i <- [0..]])
  \x b -> do
    let args_ = args' x
    let lets   = map (fromJust . fst) $ filter (isJust . fst) args_
    let cs = map (fromJust . snd) $ filter (isJust . snd) args_
    let cond  = IRApplication (IRVariable "eq") [IRDictAccess x "type", IRLiteral (String n)]
        conds = case cs of
                  (c:cs') -> cond `and` foldl and c cs'
                  _ -> cond
      in IRIf conds (lets ++ b)
compileCase PWildcard = do
  \_ b -> IRBlock b
compileCase (PLiteral l) = do
  \x b -> 
    let cond = IRApplication (IRVariable "eq") [x, IRLiteral l]
      in IRIf cond b
compileCase (PList pats) = do
  \x b -> do
    let args_ = concat $ zipWith (\i y -> do
          let f = findPattern y
          f (IRListAccess x (IRLiteral (Int i)))) [0..] pats
    let lets   = map (fromJust . fst) $ filter (isJust . fst) args_
    let cond = IRApplication (IRVariable "eq") [IRApplication (IRVariable "Array_length") [IRList [x]], IRLiteral (Int $ toInteger (length pats))]
    let cs = map (fromJust . snd) $ filter (isJust . snd) args_
    let conds = createAnd $ cond : cs
      in IRIf conds (lets ++ b)
compileCase (PStructure fields) = do
  \x b -> do
    let args_ = concat $ map (\(n, y) -> do
          let f = findPattern y
          f (IRDictAccess x n) ++ concatMap (\(x', _) -> [(Nothing, Just $ IRIn x x')]) fields ) fields
    let lets = map (fromJust . fst) $ filter (isJust . fst) args_
    let cs = map (fromJust . snd) $ filter (isJust . snd) args_
    let conds = createAnd cs
      in IRIf conds (lets ++ b)

createAnd :: [IRExpression] -> IRExpression
createAnd [] = error "test"
createAnd [x] = x
createAnd (x:xs) = x `and` createAnd xs