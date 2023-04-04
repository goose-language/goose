{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Goose.CST.Annoted where

-- Representing a binding that binds a type representation
-- to a name.
data Annoted a = Annoted { annotedName :: String, annotedType :: a } 
  deriving (Eq, Ord, Functor)

pattern (:@) :: String ->  a -> Annoted a
pattern x :@ t = Annoted x t

instance Show a => Show (Annoted a) where
  show (x :@ t) = x ++ " : " ++ show t
  show _ = "COMPILER ERROR: Annoted.show"

instance {-# OVERLAPS #-} Show (Annoted String) where
  show (x :@ t) = x ++ " : " ++ t
  show _ = "COMPILER ERROR: Annoted.show"

instance {-# OVERLAPS #-} Show a => Show (Annoted (Maybe a)) where
  show (x :@ (Just t)) = x ++ " : " ++ show t
  show (x :@ Nothing) = x
  show _ = "COMPILER ERROR: Annoted.show"