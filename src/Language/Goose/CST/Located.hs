{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Goose.CST.Located where
import Text.Parsec.Pos ( SourcePos ) 

type Position = (SourcePos, SourcePos)

data Located a = Located { loc :: Position, unLoc :: a }

instance Eq a => Eq (Located a) where
  Located _ x == Located _ y = x == y

pattern (:>:) :: a -> Position -> Located a
pattern a :>: pos = Located pos a

instance Show a => Show (Located a) where
  show (Located _ x) = show x
