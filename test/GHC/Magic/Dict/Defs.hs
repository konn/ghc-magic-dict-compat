{-# LANGUAGE MultiParamTypeClasses #-}

module GHC.Magic.Dict.Defs (
  MyEq (..),
  Related (..),
  BadClass1 (..),
  BadClass2 (),
  badeq2,
  Inhabited (..),
  Given (..),
) where

class MyEq a where
  eq :: a -> a -> Bool

class Related a b where
  related :: a -> b -> Bool

class BadClass1 a where
  pos :: a -> a
  neg :: a -> a

class (MyEq a) => BadClass2 a

badeq2 :: (BadClass2 a) => a -> a -> Bool
badeq2 = eq

class Inhabited a where
  inhibitant :: a

class Given a where
  given :: a
