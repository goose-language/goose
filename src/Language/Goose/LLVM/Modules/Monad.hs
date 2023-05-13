{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Goose.LLVM.Modules.Monad where
import qualified LLVM.IRBuilder as IRB
import qualified Control.Monad.RWS as ST
import qualified Data.Map as M
import qualified LLVM.AST as AST

type LLVM m = (IRB.MonadModuleBuilder m, ST.MonadState (M.Map String AST.Operand, Int) m, IRB.MonadIRBuilder m)