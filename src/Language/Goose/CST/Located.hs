{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
module Language.Goose.CST.Located where
import Text.Parsec.Pos ( SourcePos ) 

type Position = (SourcePos, SourcePos)

data Located a = Located { loc :: Position, unLoc :: a }
  deriving (Eq, Ord, Functor)

pattern (:>:) :: a -> Position -> Located a
pattern a :>: pos = Located pos a

instance Show a => Show (Located a) where
  show (Located _ x) = show x
