{-# LANGUAGE FlexibleContexts #-}
module Language.Goose.Module.Bundler where
import Language.Goose.Module.Monad
import Language.Goose.CST.Expression
import Language.Goose.CST.Located

import qualified Control.Monad.Except as E
import qualified Data.Map as M
import qualified Data.List as L
import qualified Control.Monad.State as ST
import qualified Language.Goose.CST.Modules.Declaration as D

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

checkToplevel :: [String] -> Located (Toplevel) -> [String]
checkToplevel names (Public (Function { functionName = name } :>: _) :>:_) = if name `elem` names
  then filter (/= name) names
  else names
checkToplevel names (Public (Enumeration { enumerationName = name } :>: _) :>:_) = if name `elem` names
  then filter (/= name) names
  else names
checkToplevel names (Namespace _ toplevels :>: _) = concatMap (checkToplevel names) toplevels
checkToplevel names (Public toplevel :>: _) = checkToplevel names toplevel
checkToplevel names _ = names

checkModule :: [String] -> [Located (Toplevel)] -> [String]
checkModule = foldl checkToplevel

createName :: MonadBundling m => String -> m String
createName name = do
  paths <- ST.gets currentPaths
  return $ (if not (null paths) then L.intercalate "_" (reverse paths) ++ "_" else "") ++ name

analyseToplevel :: MonadBundling m => Located (Toplevel) -> m [Located (Toplevel)]
analyseToplevel (Function name args body :>: pos) = do
  (name', new) <- do
    if name == "main"
      then return ("main", "main")
      else do
        name' <- createName name
        return (name', name')
  let env' = M.fromList $ zip args args
  body' <- local' (\s -> s { mappings = M.insert name' new (M.union env' (mappings s)) }) $ do
    resolveImportedExpressions body
  return [Function new args body' :>: pos]
analyseToplevel (Namespace name toplevels :>: _) = do
  case name of
    "_" -> keepPublic . concat <$> mapM analyseToplevel toplevels
    _ -> do
      ST.modify $ \s -> s { currentPaths = currentPaths s ++ [name] }
      toplevels' <- concat <$> mapM analyseToplevel toplevels
      ST.modify $ \s -> s { currentPaths = init $ currentPaths s }
      return $ keepPublic toplevels'
analyseToplevel (Extern name gens decl ret :>: pos) = do
  decl' <- mapM resolveImportedType decl
  ret' <- resolveImportedType ret
  name' <- createName name
  ST.modify $ \s -> s { mappings = M.insert name' name (mappings s) }
  return [Extern name gens decl' ret' :>: pos]
analyseToplevel (Declare name gens decl ret :>: pos) = do
  decl' <- mapM resolveImportedType decl
  ret' <- resolveImportedType ret
  name' <- createName name
  ST.modify $ \s -> s { mappings = M.insert name' name' (mappings s) }
  return [Declare name' gens decl' ret' :>: pos]
analyseToplevel (Public toplevel :>: pos) = map (Located pos . Public) <$> analyseToplevel toplevel
analyseToplevel x = return [x]

keepPublic :: [Located (Toplevel)] -> [Located (Toplevel)]
keepPublic = 
    map (\x'@(Located _ x) -> case x of
      Public x'' -> x''
      _ -> x')

keep :: Ord k => M.Map k a -> [k] -> M.Map k a
keep m ks = M.fromList $ filter (\(k, _) -> k `elem` ks) $ M.toList m

lookupModule :: MonadBundling m => Maybe [String] -> [Located Toplevel] -> m [Located (Toplevel)]
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
makeName (D.Namespaced paths name) = (if not (null paths) then L.intercalate "_" paths ++ "_" else "") ++ name
makeName (D.Simple name) = name

getName :: D.Namespaced -> D.Name
getName (D.Namespaced _ name) = name
getName (D.Simple name) = name

resolveImportedType :: MonadBundling m => D.Declaration -> m D.Declaration
resolveImportedType (D.ID name) = do
  mappings' <- ST.gets types
  case M.lookup (makeName name) mappings' of
    Just name' -> return $ D.ID (D.Simple name')
    Nothing -> return $ D.ID name
resolveImportedType (D.List t) = do
  t' <- resolveImportedType t
  return $ D.List t'
resolveImportedType x = return x

resolveImportedExpressions :: MonadBundling m => Located (Expression) -> m (Located (Expression))
resolveImportedExpressions (Variable name :>: pos) = do
  mappings' <- ST.gets mappings
  case M.lookup (makeName name) mappings' of
    Just name' -> return $ Variable (D.Simple name') :>: pos
    Nothing -> E.throwError ("Variable " ++ show name ++ " is not defined", pos)
resolveImportedExpressions (Application n args :>: pos) = do
  n' <- resolveImportedExpressions n
  args' <- mapM resolveImportedExpressions args
  return $ Application n' args' :>: pos
resolveImportedExpressions (Lambda args body :>: pos) = do
  body' <- local' (\s -> s { mappings = M.union (M.fromList $ zip args args) (mappings s) }) $ do
    resolveImportedExpressions body
  return $ Lambda args body' :>: pos
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
resolveImportedExpressions (Let n expr body :>: pos) = do
  expr' <- resolveImportedExpressions expr
  ST.modify (\s -> s { mappings = M.insert n n (mappings s) })
  body' <- resolveImportedExpressions body
  return $ Let n expr' body' :>: pos
resolveImportedExpressions (Dereference e :>: pos) = do
  e' <- resolveImportedExpressions e
  return $ Dereference e' :>: pos
resolveImportedExpressions (While cond body :>: pos) = do
  cond' <- resolveImportedExpressions cond
  body' <- mapM resolveImportedExpressions body
  return $ While cond' body' :>: pos
resolveImportedExpressions (For name list body :>: pos) = do
  from' <- resolveImportedExpressions list
  body' <- local' (\s -> s { mappings = M.insert name name (mappings s) }) $ mapM resolveImportedExpressions body
  return $ For name from' body' :>: pos
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
resolveImportedExpressions (Located pos _) = E.throwError ("Not implemented", pos)

resolveImportedUpdate :: MonadBundling m => Located Updated -> m (Located Updated)
resolveImportedUpdate (VariableUpdate name :>: pos) = do
  mappings' <- ST.gets mappings
  case M.lookup (makeName name) mappings' of
    Just name' -> return $ VariableUpdate (D.Simple name') :>: pos
    Nothing -> return $ VariableUpdate name :>: pos
resolveImportedUpdate (ListUpdate name index :>: pos) = do
  name' <- resolveImportedUpdate name
  index' <- resolveImportedExpressions index
  return $ ListUpdate name' index' :>: pos
resolveImportedUpdate (StructureUpdate name field :>: pos) = do
  name' <- resolveImportedUpdate name
  return $ StructureUpdate name' field :>: pos
resolveImportedUpdate (Located pos _) = E.throwError ("Not implemented", pos)

runModuleBundling :: (E.MonadIO m, MonadFail m) => [Located Toplevel] -> m (Either (String, Position) ([Located Toplevel]))
runModuleBundling toplevel = do
  E.runExceptT (ST.evalStateT (lookupModule Nothing toplevel) (BundlingState [] mempty mempty 0 []))