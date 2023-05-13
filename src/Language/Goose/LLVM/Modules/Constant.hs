{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Language.Goose.LLVM.Modules.Constant where
import Language.Goose.LLVM.Modules.Monad (LLVM)
import qualified LLVM.AST as AST
import Data.Bits
import qualified LLVM.IRBuilder as IRB
import qualified LLVM.AST.Type as AST
import LLVM.AST (Operand(ConstantOperand))
import qualified LLVM.AST.Float as C
import qualified LLVM.AST.Constant as IRB

#define MASK_SIGN        0x8000000000000000
#define MASK_EXPONENT    0x7ff0000000000000
#define MASK_QUIET       0x0008000000000000
#define MASK_TYPE        0x0007000000000000
#define MASK_SIGNATURE   0xffff000000000000
#define MASK_PAYLOAD_PTR 0x0000ffffffffffff
#define MASK_PAYLOAD_INT 0x00000000ffffffff

-- Type IDs for short encoded types
#define MASK_TYPE_NAN     0x0000000000000000
#define MASK_TYPE_FALSE   0x0001000000000000
#define MASK_TYPE_TRUE    0x0002000000000000
#define MASK_TYPE_NULL    0x0003000000000000
#define MASK_TYPE_INTEGER 0x0004000000000000
#define MASK_TYPE_CHAR    0x0005000000000000

-- Constant short encoded values
#define kNaN   (MASK_EXPONENT .|. MASK_QUIET)
#define kFalse (kNaN .|. MASK_TYPE_FALSE)
#define kTrue  (kNaN .|. MASK_TYPE_TRUE)
#define kNull  (kNaN .|. MASK_TYPE_NULL)

-- Signatures of encoded types
#define SIGNATURE_NAN     kNaN
#define SIGNATURE_FALSE   kFalse
#define SIGNATURE_TRUE    kTrue
#define SIGNATURE_NULL    kNull
#define SIGNATURE_INTEGER (kNaN .|. MASK_TYPE_INTEGER)
#define SIGNATURE_CHAR    (kNaN .|. MASK_TYPE_CHAR)
#define SIGNATURE_POINTER (kNaN .|. MASK_SIGN)

integer :: LLVM m => Integer -> m AST.Operand
integer i = 
  IRB.or (IRB.int64 SIGNATURE_INTEGER) (IRB.int64 i)

char :: LLVM m => Char -> m AST.Operand
char c = 
  IRB.or (IRB.int64 SIGNATURE_CHAR) (IRB.int64 (toInteger (fromEnum c)))

float :: LLVM m => Double -> m AST.Operand
float f = do
  x <- IRB.alloca AST.double Nothing 0
  IRB.store x 0 (ConstantOperand $ IRB.Float (C.Double f))
  bc <- IRB.bitcast x (AST.ptr AST.i64)
  IRB.load bc 0

unit :: LLVM m => m AST.Operand
unit = return $ IRB.int64 kNaN

true :: LLVM m => m AST.Operand
true = return $ IRB.int64 kTrue

false :: LLVM m => m AST.Operand
false = return $ IRB.int64 kFalse
