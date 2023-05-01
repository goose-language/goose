{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Language.Goose.LLVM.Compiler where
import Data.String
import qualified LLVM.AST.Constant as C
import LLVM.AST (Operand(ConstantOperand))
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST as AST
import qualified Language.Goose.CLang.Definition.IR as IR
import qualified Language.Goose.CST.Literal as L
import qualified LLVM.Context as LL
import qualified LLVM as LL
import qualified LLVM.PassManager as LL
import qualified Data.ByteString.Char8 as C
import qualified LLVM.IRBuilder as IRB
import qualified Control.Monad.State as ST
import qualified Data.Map as M
import qualified Control.Arrow as BF
import qualified LLVM.AST.Float as C
import LLVM.AST.Constant (Constant(Float))
import GHC.Float (double2Float)
import Data.Char (ord)
import qualified LLVM.AST.Typed as AST
import qualified LLVM.AST.IntegerPredicate as IP
import Data.Functor

type LLVM m = (IRB.MonadModuleBuilder m, ST.MonadState (M.Map String AST.Operand, Int) m, IRB.MonadIRBuilder m)

retVoid :: LLVM m => m ()
retVoid = do
  unit <- IRB.call (AST.ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i64 [] False) "unit") []
  IRB.ret unit

fresh :: LLVM m => m Int
fresh = do
  (m, i) <- ST.get
  ST.put (m, i + 1)
  return i

local :: LLVM m => m a -> m a
local m = do
  (m', _) <- ST.get
  a <- m
  (_, i') <- ST.get
  ST.put (m', i')
  return a

valuesTy :: M.Map String AST.Type
valuesTy = M.fromList [
    -- Value constructors
    ("integer", AST.FunctionType AST.i64 [AST.i64] False),
    ("character", AST.FunctionType AST.i64 [AST.i8] False),
    ("unit", AST.FunctionType AST.i64 [] False),
    ("boolean", AST.FunctionType AST.i64 [AST.i1] False),
    ("emptyList", AST.FunctionType AST.i64 [] False),
    ("floating", AST.FunctionType AST.i64 [AST.float] False),
    ("string", AST.FunctionType AST.i64 [AST.ptr AST.i8] False),
    ("list", AST.FunctionType AST.i64 [AST.i64] True),
    ("makeLambda", AST.FunctionType AST.i64 [funTy] False),
    ("structure", AST.FunctionType AST.i64 [AST.i64] True),

    -- Equality operators
    ("eq", AST.FunctionType AST.i64 [AST.i64, AST.i64] False),
    ("neq", AST.FunctionType AST.i64 [AST.i64, AST.i64] False),

    -- Value access functions
    ("decode_lambda", AST.FunctionType funTy [AST.i64] False),
    ("decode_integer", AST.FunctionType AST.i64 [AST.i64] False),
    ("decode_character", AST.FunctionType AST.i8 [AST.i64] False),
    ("decode_unit", AST.FunctionType AST.i64 [AST.i64] False),
    ("decode_floating", AST.FunctionType AST.float [AST.i64] False),
    ("decode_string", AST.FunctionType (AST.ptr AST.i8) [AST.i64] False),
    ("decode_boolean", AST.FunctionType AST.i1 [AST.i64] False),
    ("index_", AST.FunctionType AST.i64 [AST.i64, AST.i64] False),
    ("property_", AST.FunctionType AST.i64 [AST.i64, AST.ptr AST.i8] False),

    -- Comparison operators
    ("lt", AST.FunctionType AST.i64 [AST.i64, AST.i64] False),
    ("gt", AST.FunctionType AST.i64 [AST.i64, AST.i64] False),
    ("lte", AST.FunctionType AST.i64 [AST.i64, AST.i64] False),
    ("gte", AST.FunctionType AST.i64 [AST.i64, AST.i64] False),

    -- Arithmetic operators
    ("add", AST.FunctionType AST.i64 [AST.i64, AST.i64] False),
    ("subtract", AST.FunctionType AST.i64 [AST.i64, AST.i64] False),
    ("multiply", AST.FunctionType AST.i64 [AST.i64, AST.i64] False),
    ("divide", AST.FunctionType AST.i64 [AST.i64, AST.i64] False),

    ("length", AST.FunctionType AST.i64 [AST.i64] False),
    ("in", AST.FunctionType AST.i64 [AST.i64, AST.ptr AST.i8] False),

    -- Update functions
    ("update_index", AST.FunctionType AST.void [AST.i64, AST.i64, AST.i64] False),
    ("update_property", AST.FunctionType AST.void [AST.i64, AST.ptr AST.i8, AST.i64] False)
  ]

funTy :: AST.Type
funTy = AST.ptr $ AST.FunctionType AST.i64 [AST.i64] False

string :: LLVM m => String -> m AST.Operand
string s = do
  i <- fresh
  let name = "string" ++ show i
  AST.ConstantOperand <$> IRB.globalStringPtr s (AST.Name $ fromString name)

generateValueDefinitions :: LLVM m => m ()
generateValueDefinitions =
  ST.forM_ (M.toList valuesTy) $ \(name, z@(AST.FunctionType ret args isVar)) -> do
  ST.modify . BF.first $ M.insert name $ ConstantOperand $ C.GlobalReference (AST.ptr z) (AST.Name $ fromString name)
  if isVar
    then IRB.externVarArgs (AST.Name $ fromString name) args ret
    else IRB.extern (AST.Name $ fromString name) args ret

namedBlock :: IRB.MonadIRBuilder m => AST.Name -> m AST.Name
namedBlock nm = do
  IRB.emitBlockStart nm
  return nm

convertToplevel :: LLVM m => IR.IRToplevel -> m ()
convertToplevel (IR.IRFunction name args body) = do
  let argTypes = [AST.i64]
  let retType = AST.i64
  let ftype = AST.ptr $ AST.FunctionType retType argTypes False
  ST.modify . BF.first $ M.insert name $ ConstantOperand $ C.GlobalReference ftype (AST.Name $ fromString name)
  IRB.function (AST.Name $ fromString name) args' AST.i64 $ \args'' -> local $ do
    let argsToDestruct = head args''
    ST.forM_ (zip args [0..]) $ \(argName, i') -> do
      i <- IRB.alloca AST.i64 Nothing 0
      ST.modify . BF.first $ M.insert argName i
      indexed <- IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i64 [AST.i64, AST.i64] False) (AST.Name "index_")) [(argsToDestruct, []), (ConstantOperand $ C.Int 64 i', [])]
      IRB.store i 0 indexed
    mapM_ convertStatement body
    b <- IRB.hasTerminator
    ST.unless b retVoid
    ST.when (name == "main") $ IRB.ret $ ConstantOperand $ C.Int 64 0
  return ()
  where args' = [(AST.i64, IRB.ParameterName $ fromString "args")]
convertToplevel (IR.IRDeclare name args _) = do
  case args of
    Just x -> do
      let argTypes = map (const AST.i64) x
      let retType = AST.i64
      let ftype = AST.ptr $ AST.FunctionType retType argTypes False
      IRB.extern (AST.Name $ fromString name) [AST.i64] AST.i64
      ST.modify . BF.first $ M.insert name $ ConstantOperand $ C.GlobalReference ftype (AST.Name $ fromString name)
    Nothing -> do
      IRB.global (AST.Name $ fromString name) AST.i64 $ C.Int 64 0
      ST.modify . BF.first $ M.insert name $ ConstantOperand $ C.GlobalReference (AST.ptr AST.i64) (AST.Name $ fromString name)
convertToplevel _ = return ()

convertStatement :: LLVM m => IR.IRStatement -> m ()
convertStatement (IR.IRReturn expr) = do
  expr' <- convertExpression expr
  IRB.ret expr'
convertStatement (IR.IRDeclarationStatement name epxr) = do
  i <- IRB.alloca AST.i64 Nothing 0
  expr' <- convertExpression epxr
  ST.modify . BF.first $ M.insert name i
  IRB.store i 0 expr'
convertStatement (IR.IRExpression expr) = do
  void $ convertExpression expr
convertStatement (IR.IRIf cond then') = do
  cond' <- convertExpression cond
  condConversion <- IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i1 [AST.i64] False) (AST.Name $ fromString "decode_boolean")) [(cond', [])]

  thenBlName <- IRB.freshName "if.then"
  mergeBlName <- IRB.freshName "if.exit"

  IRB.condBr condConversion thenBlName mergeBlName

  namedBlock thenBlName
  mapM_ convertStatement then'
  IRB.br mergeBlName

  void $ namedBlock mergeBlName
convertStatement (IR.IRIfElse cond then' else') = do
  cond' <- convertExpression cond
  condConversion <- IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i1 [AST.i64] False) (AST.Name $ fromString "decode_boolean")) [(cond', [])]

  thenBlName <- IRB.freshName "if.then"
  elseBlName <- IRB.freshName "if.else"
  mergeBlName <- IRB.freshName "if.exit"

  IRB.condBr condConversion thenBlName elseBlName

  namedBlock thenBlName
  mapM_ convertStatement then'
  IRB.br mergeBlName

  namedBlock elseBlName
  mapM_ convertStatement else'
  IRB.br mergeBlName

  void $ namedBlock mergeBlName
convertStatement (IR.IRFor name list exprs) = do
  -- Should compile to something like:
  -- for (int i = 0; i < length; i++) {
  --   int x = list[i];
  --   exprs
  -- }

  -- Create the loop
  condBlName <- IRB.freshName "cond"
  bodyBlName <- IRB.freshName "body"
  mergeBlName <- IRB.freshName "merge"
  list' <- convertExpression list

  i <- IRB.alloca AST.i64 Nothing 0
  IRB.store i 0 $ ConstantOperand $ C.Int 64 0
  IRB.br condBlName
  
  -- Create the condition block
  namedBlock condBlName
  i' <- IRB.load i 0
  length' <- IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i64 [AST.i64] False) (AST.Name $ fromString "length")) [(list', [])]
  cond' <- IRB.icmp IP.SLT i' length'
  IRB.condBr cond' bodyBlName mergeBlName

  -- Create the body block
  namedBlock bodyBlName
  x <- IRB.alloca AST.i64 Nothing 0
  ST.modify . BF.first $ M.insert name x
  integered <- IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i64 [AST.i64] False) (AST.Name $ fromString "integer")) [(i', [])]
  call <- IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i64 [AST.i64, AST.i64] False) (AST.Name $ fromString "index_")) [(list', []), (integered, [])]
  IRB.store x 0 call
  mapM_ convertStatement exprs
  IRB.store i 0 =<< IRB.add i' (ConstantOperand $ C.Int 64 1)
  IRB.br condBlName

  -- Create the merge block
  void $ namedBlock mergeBlName
convertStatement (IR.IRWhile cond body) = do
  condBlName <- IRB.freshName "cond"
  bodyBlName <- IRB.freshName "body"
  mergeBlName <- IRB.freshName "merge"

  IRB.br condBlName

  namedBlock condBlName
  cond' <- convertExpression cond
  condConversion <- IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i1 [AST.i64] False) (AST.Name $ fromString "decode_boolean")) [(cond', [])]
  IRB.condBr condConversion bodyBlName mergeBlName

  namedBlock bodyBlName
  mapM_ convertStatement body
  IRB.br condBlName

  void $ namedBlock mergeBlName
convertStatement (IR.IRBlock stmts) = do
  local $ mapM_ convertStatement stmts
convertStatement (IR.IRUpdate (IR.IRVariable name) expr) = do
  expr' <- convertExpression expr
  i <- ST.gets $ M.findWithDefault (error $ "Variable " ++ name ++ " not found") name . fst
  IRB.store i 0 expr'
convertStatement (IR.IRUpdate (IR.IRListAccess lst idx) expr) = do
  expr' <- convertExpression expr
  lst' <- convertExpression lst
  idx' <- convertExpression idx
  integered <- IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i64 [AST.i64] False) (AST.Name $ fromString "decode_integer")) [(idx', [])]
  IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.void [AST.i64, AST.i64, AST.i64] False) (AST.Name $ fromString "update_index")) [(lst', []), (integered, []), (expr', [])]
  return ()
convertStatement (IR.IRUpdate (IR.IRDictAccess dict key) expr) = do
  expr' <- convertExpression expr
  dict' <- convertExpression dict
  key' <- string key
  IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.void [AST.i64, AST.ptr AST.i8, AST.i64] False) (AST.Name $ fromString "update_property")) [(dict', []), (key', []), (expr', [])]
  return ()
convertStatement x = error $ "Not implemented: " ++ show x

convertExpression :: LLVM m => IR.IRExpression -> m AST.Operand
convertExpression (IR.IRLiteral l) = case l of
  L.Int i -> do
    let i' = fromIntegral i
    IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i64 [AST.i64] False) (AST.Name "integer")) [(ConstantOperand $ C.Int 64 i', [])]
  L.Float f ->
    IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i64 [AST.float] False) (AST.Name "integer")) [(ConstantOperand $ Float $ C.Single (double2Float f), [])]
  L.String s -> do
    n <- string s
    IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i64 [AST.ptr AST.i8] False) (AST.Name "string")) [(n, [])]
  L.Char c -> do
    let c' = fromIntegral $ ord c
    IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i64 [AST.i8] False) (AST.Name "character")) [(ConstantOperand $ C.Int 8 c', [])]
  L.Bool b -> do
    let b' = if b then 1 else 0
    IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i64 [AST.i1] False) (AST.Name "boolean")) [(ConstantOperand $ C.Int 1 b', [])]
  L.Unit ->
    IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i64 [] False) (AST.Name "unit")) []
convertExpression (IR.IRVariable name) = do
  m <- ST.gets fst
  case M.lookup name m of
    Just x -> do
      ty <- AST.typeOf x
      case ty of
        Right (AST.PointerType AST.FunctionType {} _) -> return x
        Right _ -> IRB.load x 0
        _ -> return x
    Nothing -> error $ "Variable not found " ++ show name
convertExpression (IR.IRApplication (IR.IRVariable "property_") [dict, IR.IRLiteral (L.String key)]) = do
  dict' <- convertExpression dict
  key' <- string key
  IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i64 [AST.i64, AST.ptr AST.i8] False) (AST.Name "property_")) [(dict', []), (key', [])]
convertExpression (IR.IRApplication f args) = do
  f' <- convertExpression f
  args' <- mapM convertExpression args
  IRB.call f' $ map (,[]) args'
convertExpression (IR.IRList exprs) = do
  exprs' <- mapM convertExpression exprs
  let arrFun = ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i64 [AST.i64] True) (AST.Name "list")
  IRB.call arrFun $ (ConstantOperand $ C.Int 64 (toInteger $ length exprs), []) : map (,[]) exprs'
convertExpression (IR.IRBinary op _ _) = error $ "Should not encounter " ++ op ++ " operator in IR."
convertExpression (IR.IRTernary cond t f) = do
  cond' <- convertExpression cond
  condConversion <- IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i1 [AST.i64] False) (AST.Name $ fromString "decode_boolean")) [(cond', [])]

  t' <- convertExpression t
  f' <- convertExpression f

  IRB.select condConversion t' f'
convertExpression (IR.IRListAccess list index) = do
  list' <- convertExpression list
  index' <- convertExpression index
  IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i64 [AST.i64, AST.i64] False) (AST.Name "index_")) [(list', []), (index', [])]
convertExpression (IR.IRDictAccess dict key) = do
  dict' <- convertExpression dict
  key' <- string key
  IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i64 [AST.i64, AST.ptr AST.i8] False) (AST.Name "property_")) [(dict', []), (key', [])]
convertExpression (IR.IREUpdate (IR.IRVariable name) expr) = do
  update <- IRB.alloca AST.i64 Nothing 0
  expr' <- convertExpression expr
  IRB.store expr' 0 update
  ST.modify . BF.first $ M.insert name update
  return $ ConstantOperand $ C.Int 64 0
convertExpression (IR.IREUpdate (IR.IRListAccess lst idx) expr) = do
  expr' <- convertExpression expr
  lst' <- convertExpression lst
  idx' <- convertExpression idx
  integered <- IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i64 [AST.i64] False) (AST.Name $ fromString "decode_integer")) [(idx', [])]
  IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.void [AST.i64, AST.i64, AST.i64] False) (AST.Name $ fromString "update_index")) [(lst', []), (integered, []), (expr', [])]
convertExpression (IR.IREUpdate (IR.IRDictAccess dict key) expr) = do
  expr' <- convertExpression expr
  dict' <- convertExpression dict
  key' <- string key
  IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.void [AST.i64, AST.ptr AST.i8, AST.i64] False) (AST.Name $ fromString "update_property")) [(dict', []), (key', []), (expr', [])]
convertExpression (IR.IRDict fields) = do
  fields' <- mapM (\(k, v) -> (,) <$> string k <*> convertExpression v) fields
  let fields'' = concat [ [k, v] | (k, v) <- fields' ]
  let arrFun = ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i64 [AST.i64] True) (AST.Name "structure")
  IRB.call arrFun $ (ConstantOperand (C.Int 64 (toInteger $ length fields')), []) : map (,[]) fields''
convertExpression (IR.IRIn expr field) = do
  expr' <- convertExpression expr
  e' <- string field
  IRB.call (ConstantOperand $ C.GlobalReference (AST.ptr $ AST.FunctionType AST.i64 [AST.i64, AST.ptr AST.i8] False) (AST.Name "in")) [(expr', []), (e', [])]
convertExpression _ = return $ ConstantOperand $ C.Int 64 0

runLLVM :: [IR.IRToplevel] -> AST.Module
runLLVM xs = ST.evalState (IRB.buildModuleT "main" $ IRB.runIRBuilderT IRB.emptyIRBuilder (generateValueDefinitions *> mapM_ convertToplevel xs)) (M.empty, 0)

passes :: LL.PassSetSpec
passes = LL.defaultCuratedPassSetSpec { LL.optLevel = Just 3 }

runOpt :: AST.Module -> IO AST.Module
runOpt mod' =
  LL.withContext $ \context ->
  LL.withModuleFromAST context mod' $ \m ->
    LL.withPassManager passes $ \pm -> do
      LL.runPassManager pm m
      optmod <- LL.moduleAST m
      s <- LL.moduleBitcode m
      putStrLn (C.unpack s)
      return optmod

runCompiler :: [IR.IRToplevel] -> IO String
runCompiler xs = do
  -- mapM_ traceShowM xs
  let mod' = runLLVM xs
  s <- LL.withContext $ \context ->
    LL.withModuleFromAST context mod' $ \m ->
      LL.moduleLLVMAssembly m
  return $ C.unpack s