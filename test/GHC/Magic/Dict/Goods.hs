{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -dcore-lint -fobject-code #-}
{-# OPTIONS_GHC -fplugin GHC.Magic.Dict.Plugin #-}

module GHC.Magic.Dict.Goods (
  eq2MyEqInt,
  neq2MyEqInt,
  eq2MyEqPoly,
  neq2MyEqPoly,
  unRelatedBy,
  give,
) where

import GHC.Magic.Dict.Compat
import GHC.Magic.Dict.Defs

eq2MyEqInt :: Int -> Int -> Bool
eq2MyEqInt = withDict @(MyEq Int) ((==) @Int) eq

neq2MyEqInt :: Int -> Int -> Bool
neq2MyEqInt = withDict @(MyEq Int) ((/=) @Int) eq

eq2MyEqPoly :: forall a. (Eq a) => a -> a -> Bool
eq2MyEqPoly = withDict @(MyEq a) ((==) @a) eq

neq2MyEqPoly :: forall a. (Eq a) => a -> a -> Bool
neq2MyEqPoly = withDict @(MyEq a) ((/=) @a) eq

unRelatedBy :: forall a b. (a -> b -> Bool) -> a -> b -> Bool
unRelatedBy rel = withDict @(Related a b) (fmap not . rel) related

give :: forall a r. a -> ((Given a) => r) -> r
give = withDict @(Given a)
