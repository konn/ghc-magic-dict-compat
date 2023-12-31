{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans -ddump-tc-trace #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors -dcore-lint #-}
{-# OPTIONS_GHC -fplugin GHC.Magic.Dict.Plugin #-}

module GHC.Magic.Dict.Errors (badClass1, badClass2, badVoid, withEqFail) where

import Data.Void (Void)
import GHC.Magic.Dict.Compat
import GHC.Magic.Dict.Defs

withEqFail :: forall a r. (a -> a -> Bool) -> ((Eq a) => r) -> r
withEqFail = withDict @(Eq a)

badClass1 :: Int -> Int
badClass1 = withDict @(BadClass1 Int) (id @Int) neg

badClass2 :: Int -> Int -> Bool
badClass2 = withDict @(BadClass2 Int) ((==) @Int) badeq2

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 904
instance WithDict (Inhabited Void) ()
#endif

badVoid :: Void
badVoid = withDict @(Inhabited Void) () (inhibitant @Void)
