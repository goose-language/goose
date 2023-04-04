{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Goose.CST.Literal where

data Literal
  = Char Char
  | String String
  | Int Integer
  | Float Double
  | Bool Bool
  | Unit
  deriving (Eq, Ord)

instance Show Literal where
  show (Char c) = show c
  show (String s) = show s
  show (Int i) = show i
  show (Float f) = show f
  show (Bool b) = show b
  show Unit = "()"