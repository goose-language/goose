{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Language.Goose.Module.Bundler where
import Language.Goose.Module.Monad
import Language.Goose.CST.Expression
import Language.Goose.CST.Modules.Pattern
import Language.Goose.CST.Located

import qualified Control.Monad.Except as E
import qualified Data.Map as M
import qualified Data.List as L
import qualified Control.Monad.State as ST
import qualified Language.Goose.CST.Modules.Declaration as D
import qualified Language.Goose.CST.Annoted as C

import Control.Arrow
import Data.Functor

local' :: MonadBundling m => (BundlingState -> BundlingState) -> m a -> m a
local' f m = do
  s <- ST.get
  ST.modify f
  a <- m
  ST.modify $ \s' -> s' { mappings = mappings s' `M.union` mappings s, types = types s' `M.union` types s }
  return a

local :: MonadBundling m => (BundlingState -> BundlingState) -> m a -> m a
local f m = do
  s <- ST.get
  ST.modify f
  a <- m
  ST.modify $ \s' -> s' { mappings = mappings s, types = types s, currentPaths = currentPaths s }
  return a

-- | Check for duplicate definitions.

checkToplevel :: [String] -> Located Toplevel -> [String]
checkToplevel names (Public (Function { functionName = (C.Annoted name _) } :>: _) :>:_) = if name `elem` names
  then filter (/= name) names
  else names
checkToplevel names (Public (Enumeration { enumerationName = name } :>: _) :>:_) = if name `elem` names
  then filter (/= name) names
  else names
checkToplevel names (Public (Declaration { declarationName = (C.Annoted name _) } :>: _) :>:_) = if name `elem` names
  then filter (/= name) names
  else names
checkToplevel names (Namespace _ toplevels :>: _) = concatMap (checkToplevel names) toplevels
checkToplevel names (Public toplevel :>: _) = checkToplevel names toplevel
checkToplevel names _ = names

checkModule :: [String] -> [Located Toplevel] -> [String]
checkModule = foldl checkToplevel

createName :: MonadBundling m => String -> m String
createName name = do
  paths <- ST.gets currentPaths
  return $ (if not (null paths) then L.intercalate "::" paths ++ "::" else "") ++ name

analyseToplevel :: MonadBundling m => Located Toplevel -> m [Located Toplevel]
analyseToplevel (Function (C.Annoted name ret) gens args body :>: pos) = do
  (name', new) <- do
    if name == "main"
      then return ("main", "main")
      else do
        name' <- createName name
        return (name', name')
  let args' = map C.annotedName args
  tys <- mapM ((`resolveImportedMaybeType` pos) . C.annotedType) args
  ret <- resolveImportedMaybeType ret pos
  let env' = M.fromList $ zip args' args'
  body' <- local' (\s -> s { mappings = M.insert name' new (M.union env' (mappings s)) }) $
    resolveImportedExpressions body
  return [Function (C.Annoted new ret) gens (zipWith C.Annoted args' tys) body' :>: pos]
analyseToplevel (Namespace name toplevels :>: _) =
  case name of
    "_" ->
      local' (const emptyBundling) $ keepPublic . concat <$> mapM analyseToplevel toplevels
    _ -> do
      ST.modify $ \s -> s { currentPaths = currentPaths s ++ [name] }
      toplevels' <- concat <$> mapM analyseToplevel toplevels
      ST.modify $ \s -> s { currentPaths = case currentPaths s of
        [] -> []
        _ -> init $ currentPaths s }
      return $ keepPublic toplevels'
analyseToplevel (Declare name gens decl ret :>: pos) = do
  decl' <- case decl of
    Just decl -> Just <$> mapM (`resolveImportedType` pos) decl
    Nothing -> return Nothing
  ret' <- resolveImportedType ret pos
  name' <- createName name
  ST.modify $ \s -> s { mappings = M.insert name' name' (mappings s) }
  return [Declare name' gens decl' ret' :>: pos]
analyseToplevel (Public toplevel :>: pos) = map (Located pos . Public) <$> analyseToplevel toplevel
analyseToplevel (Enumeration name gens decls :>: pos) = do
  (name', new) <- do
    name' <- createName name
    return (name', name')

  ST.modify $ \s -> s { types = M.insert name' new (types s) }

  decls' <- mapM (\(C.Annoted name' ty) -> C.Annoted <$> createName name' <*> mapM (`resolveImportedType` pos) ty) decls
  let declsNames = map (C.annotedName &&& C.annotedName) decls'
  ST.modify $ \s -> s { mappings = M.union (M.fromList declsNames) (mappings s) }

  return [Enumeration new gens decls' :>: pos]
analyseToplevel (Type name gens decls :>: pos) = do
  (name', new) <- do
    name' <- createName name
    return (name', name')

  ST.modify $ \s -> s { types = M.insert name' new (types s) }

  decls' <- resolveImportedType decls pos

  return [Type new gens decls' :>: pos]
analyseToplevel (EnumDeclare name _ :>: _) = do
  name' <- createName name
  ST.modify $ \s -> s { types = M.insert name' name' (types s) }
  return []
analyseToplevel (Declaration (name C.:@ ty) expr :>: pos) = do
  ty' <- resolveImportedMaybeType ty pos
  name' <- createName name
  ST.modify $ \s -> s { mappings = M.insert name' name' (mappings s) }
  expr' <- resolveImportedExpressions expr
  return [Declaration (name' C.:@ ty') expr' :>: pos]
analyseToplevel (Expression expr :>: pos) = do
  expr' <- resolveImportedExpressions expr
  return [Expression expr' :>: pos]
analyseToplevel x = return [x]

keepPublic :: [Located Toplevel] -> [Located Toplevel]
keepPublic =
    map (\x'@(Located _ x) -> case x of
      Public x'' -> x''
      _ -> x')

keep :: Ord k => M.Map k a -> [k] -> M.Map k a
keep m ks = M.fromList $ filter (\(k, _) -> k `elem` ks) $ M.toList m

lookupModule :: MonadBundling m => Maybe [String] -> [Located Toplevel] -> m [Located Toplevel]
lookupModule elements ast@(Located pos _:_) = do
  let notFound = checkModule <$> elements <*> pure ast
  case notFound of
    Just [] -> do
      ast'' <- local' (\s -> s { mappings = M.empty }) $ do
        xs <- mapM analyseToplevel ast
        ST.modify $ \s -> s { mappings = keep (mappings s) (case elements of
          Just xs' -> xs'
          _ -> M.keys $ mappings s) }
        return xs
      return $ concat ast''
    Nothing -> do
      ast'' <- local' (\s -> s { mappings = M.empty }) $ do
        xs <- mapM analyseToplevel ast
        ST.modify $ \s -> s { mappings = keep (mappings s) (case elements of
          Just xs' -> xs'
          _ -> M.keys $ mappings s) }
        return xs
      return $ concat ast''
    Just xs -> E.throwError ("Following definitions are not exported: " ++ L.intercalate ", " xs, pos)
lookupModule _ [] = return []

makeName :: D.Namespaced -> D.Name
makeName (D.Namespaced paths name) = (if not (null paths) then L.intercalate "::" paths ++ "::" else "") ++ name
makeName (D.Simple name) = name

resolveImportedType :: MonadBundling m => D.Declaration -> Position -> m D.Declaration
resolveImportedType (D.ID (D.Simple "Mutable")) _ = return $ D.ID (D.Simple "Mutable")
resolveImportedType (D.ID name) pos = do
  mappings' <- ST.gets types
  case M.lookup (makeName name) mappings' of
    Just name' -> return $ D.ID (D.Simple name')
    Nothing -> E.throwError ("Type " ++ show name ++ " is not defined", pos)
resolveImportedType (D.List t) pos = do
  t' <- resolveImportedType t pos
  return $ D.List t'
resolveImportedType (D.Constructor name args) pos = do
  name' <- resolveImportedType name pos
  args' <- mapM (`resolveImportedType` pos) args
  return $ D.Constructor name' args'
resolveImportedType (D.Function args ret) pos = do
  args' <- mapM (`resolveImportedType` pos) args
  ret' <- resolveImportedType ret pos
  return $ D.Function args' ret'
resolveImportedType (D.Structure fields) pos = do
  fields' <- mapM (mapM (`resolveImportedType` pos)) fields
  return $ D.Structure fields'
resolveImportedType x _ = return x

resolveImportedMaybeType :: MonadBundling m => Maybe D.Declaration -> Position -> m (Maybe D.Declaration)
resolveImportedMaybeType Nothing _ = return Nothing
resolveImportedMaybeType (Just t) pos = do
  t' <- resolveImportedType t pos
  return $ Just t'

resolveImportedExpressions :: MonadBundling m => Located Expression -> m (Located Expression)
resolveImportedExpressions (Variable name :>: pos) = do
  mappings' <- ST.gets mappings
  case M.lookup (makeName name) mappings' of
    Just name' -> return $ Variable (D.Simple name') :>: pos
    Nothing -> E.throwError ("Variable " ++ show name ++ " is not defined", pos)
resolveImportedExpressions (Application n args :>: pos) = do
  n' <- resolveImportedExpressions n
  args' <- mapM resolveImportedExpressions args
  return $ Application n' args' :>: pos
resolveImportedExpressions (Lambda args ret body :>: pos) = do
  let names = map C.annotedName args
  tys <- mapM ((`resolveImportedMaybeType` pos) . C.annotedType) args
  body' <- local' (\s -> s { mappings = M.union (M.fromList $ zip names names) (mappings s) }) $
    resolveImportedExpressions body
  ret' <- resolveImportedMaybeType ret pos
  return $ Lambda (zipWith C.Annoted names tys) ret' body' :>: pos
resolveImportedExpressions (If cond t f :>: pos) = do
  cond' <- resolveImportedExpressions cond
  t' <- resolveImportedExpressions t
  f' <- case f of
    Just f'' -> Just <$> resolveImportedExpressions f''
    Nothing -> return Nothing
  return $ If cond' t' f' :>: pos
resolveImportedExpressions (Sequence exprs :>: pos) = do
  exprs' <- local id $ mapM resolveImportedExpressions exprs
  return $ Sequence exprs' :>: pos
resolveImportedExpressions (List exprs :>: pos) = do
  exprs' <- mapM resolveImportedExpressions exprs
  return $ List exprs' :>: pos
resolveImportedExpressions (ListAccess arr index :>: pos) = do
  arr' <- resolveImportedExpressions arr
  index' <- resolveImportedExpressions index
  return $ ListAccess arr' index' :>: pos
resolveImportedExpressions (Let (C.Annoted n t) expr body :>: pos) = do
  ST.modify (\s -> s { mappings = M.insert n n (mappings s) })
  t <- resolveImportedMaybeType t pos
  expr' <- resolveImportedExpressions expr
  body' <- resolveImportedExpressions body
  return $ Let (C.Annoted n t) expr' body' :>: pos
resolveImportedExpressions (While cond body :>: pos) = do
  cond' <- resolveImportedExpressions cond
  body' <- mapM resolveImportedExpressions body
  return $ While cond' body' :>: pos
resolveImportedExpressions (For (C.Annoted name t) list body :>: pos) = do
  from' <- resolveImportedExpressions list
  t <- resolveImportedMaybeType t pos
  body' <- local' (\s -> s { mappings = M.insert name name (mappings s) }) $ mapM resolveImportedExpressions body
  return $ For (C.Annoted name t) from' body' :>: pos
resolveImportedExpressions (Update update expr :>: pos) = do
  update' <- resolveImportedUpdate update
  expr' <- resolveImportedExpressions expr
  return $ Update update' expr' :>: pos
resolveImportedExpressions (Literal l :>: pos) = return $ Literal l :>: pos
resolveImportedExpressions (Return e :>: pos) = do
  e' <- resolveImportedExpressions e
  return $ Return e' :>: pos
resolveImportedExpressions (Binary op e1 e2 :>: pos) = do
  e1' <- resolveImportedExpressions e1
  e2' <- resolveImportedExpressions e2
  return $ Binary op e1' e2' :>: pos
resolveImportedExpressions (Structure fields :>: pos) = do
  fields' <- mapM (\(name, expr) -> (,) name <$> resolveImportedExpressions expr) fields
  return $ Structure fields' :>: pos
resolveImportedExpressions (StructureAccess expr name :>: pos) = do
  expr' <- resolveImportedExpressions expr
  return $ StructureAccess expr' name :>: pos
resolveImportedExpressions (Match expr cases :>: pos) = do
  expr' <- resolveImportedExpressions expr
  cases' <- mapM (\(Located pos pattern, expr) -> do
    pattern' <- resolveImportedPattern pattern <&> (:>: pos)
    expr' <- resolveImportedExpressions expr
    return (pattern', expr')) cases
  return $ Match expr' cases' :>: pos
resolveImportedExpressions (Located pos _) = E.throwError ("Not implemented", pos)

resolveImportedPattern :: MonadBundling m => Pattern -> m Pattern
resolveImportedPattern (VariablePattern name) = do
  mappings' <- ST.gets mappings
  case M.lookup (makeName name) mappings' of
    Just name' -> return $ VariablePattern (D.Simple name')
    Nothing -> do
      ST.modify (\s -> s { mappings = M.insert (makeName name) (makeName name) (mappings s) })
      return $ VariablePattern name
resolveImportedPattern (ListPattern names) = do
  names' <- mapM (\(Located pos n) -> do
    n' <- resolveImportedPattern n
    return $ n' :>: pos) names
  return $ ListPattern names'
resolveImportedPattern (StructurePattern fields) = do
  fields' <- mapM (\(name, Located pos expr) -> do
    expr' <- resolveImportedPattern expr
    return (name, expr' :>: pos)) fields
  return $ StructurePattern fields'
resolveImportedPattern (LiteralPattern l) = return $ LiteralPattern l
resolveImportedPattern WildcardPattern = return WildcardPattern
resolveImportedPattern (ConstructorPattern name pats) = do
  mappings' <- ST.gets mappings
  name <- case M.lookup (makeName name) mappings' of
    Just name' -> return (D.Simple name')
    Nothing -> return name
  pats' <- mapM (\(Located pos n) -> resolveImportedPattern n <&> (:>: pos)) pats
  return $ ConstructorPattern name pats'

resolveImportedUpdate :: MonadBundling m => Located Updated -> m (Located Updated)
resolveImportedUpdate (VariableUpdate name :>: pos) = do
  mappings' <- ST.gets mappings
  case M.lookup (makeName name) mappings' of
    Just name' -> return $ VariableUpdate (D.Simple name') :>: pos
    Nothing -> return $ VariableUpdate name :>: pos
resolveImportedUpdate (ListUpdate arr index :>: pos) = do
  arr' <- resolveImportedUpdate arr
  index' <- resolveImportedExpressions index
  return $ ListUpdate arr' index' :>: pos
resolveImportedUpdate (StructureUpdate expr name :>: pos) = do
  expr' <- resolveImportedUpdate expr
  return $ StructureUpdate expr' name :>: pos
resolveImportedUpdate (Located pos _) = E.throwError ("Not implemented", pos)

runModuleBundling :: (E.MonadIO m, MonadFail m) => [Located Toplevel] -> m (Either (String, Position) [Located Toplevel])
runModuleBundling toplevel =
  E.runExceptT (ST.evalStateT (lookupModule Nothing toplevel) (BundlingState [] mempty mempty 0 []))