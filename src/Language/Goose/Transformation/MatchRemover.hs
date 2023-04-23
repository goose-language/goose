{-# LANGUAGE LambdaCase #-}
module Language.Goose.Transformation.MatchRemover where
import Language.Goose.Typecheck.Definition.AST
import Language.Goose.Transformation.ANF.AST
import qualified Language.Goose.CST.Literal as C
import Language.Goose.Transformation.ANF.ANF (v)
import qualified Control.Arrow as BF

findPattern :: Pattern -> (ANFExpression -> [(Maybe ANFStatement, Maybe ANFExpression)])
findPattern (PLiteral l) = \expr -> [(Nothing, Just $ EBinary "==" expr $ ELiteral l)]
findPattern PWildcard = const [(Nothing, Nothing)]
findPattern (PVariable v' _) = \expr -> [(Just $ SLet v' expr, Nothing)]
findPattern (PConstructor name xs) = \expr -> do
  let xs' = zipWith (\x y -> findPattern y (EStructAccess expr x)) (["a" ++ show i | i <- [(0 :: Integer)..]]) xs
  concat ([(Nothing, Just $ EBinary "==" (EStructAccess expr "type") $ ELiteral (C.String name))] : xs')
findPattern (PList pats) = \expr -> do
  let pats' = map findPattern pats
  let pats'' = zipWith (\i f -> f (EListAccess expr (ELiteral $ C.Int i))) [0..] pats'
  concat pats'' ++ [(Nothing, Just $ EBinary "==" (EApplication (EVariable "Array::length" v) [expr]) (ELiteral $ C.Int $ toInteger $ length pats))]
findPattern (PStructure fields) = \expr -> do
  let args' = map (BF.second findPattern) fields
  let args'' = concatMap (\(x, f) -> f (EStructAccess expr x)) args'
  concatMap (\(x, _) -> [(Nothing, Just $ EApplication (EVariable "Array::has" v) [expr, ELiteral $ C.String x])]) fields ++ args''

createIfSequence :: [ANFStatement]-> ANFStatement
createIfSequence [] = error "Empty if sequence"
createIfSequence [x] = x
createIfSequence (SIf cond then' _:xs) = SIf cond then' [createIfSequence xs]
createIfSequence _ = error "Invalid if sequence"

removeMatch :: ANFStatement -> [ANFStatement]
removeMatch (SMatch expr cases) = do
  let decl = SLet "$$match$$" expr
  let var = EVariable "$$match$$" v
  let cases' = map (\(pat, body) -> (findPattern pat var, body)) cases
  let ifs = concatMap (\(pats, body) -> do
            let (decls, conds) = unzip pats
            let decls' = concatMap (\case
                  Just x -> [x]
                  Nothing -> []) decls
            let conds' = concatMap (\case
                  Just x -> [x]
                  Nothing -> []) conds
            if null conds' then
              decls' ++ body
            else
              [SIf (foldl1 (EBinary "&&") conds') (decls' ++ body) []]
          ) cases'
  [decl, createIfSequence ifs]
removeMatch (SExpression expr) = [SExpression expr]
removeMatch (SIf cond then' else') = [SIf cond (concatMap removeMatch then') (concatMap removeMatch else')]
removeMatch (SWhile cond body) = [SWhile cond (concatMap removeMatch body)]
removeMatch (SFor var list body) = [SFor var list (concatMap removeMatch body)]
removeMatch (SLet name expr) = [SLet name expr]
removeMatch (SReturn expr) = [SReturn expr]
removeMatch x = [x]

removeMatchTL :: ANFDefinition -> ANFDefinition
removeMatchTL (DFunction name args body) = DFunction name args (concatMap removeMatch body)
removeMatchTL (DDeclaration name expr) = DDeclaration name expr
removeMatchTL (DDeclare name) = DDeclare name