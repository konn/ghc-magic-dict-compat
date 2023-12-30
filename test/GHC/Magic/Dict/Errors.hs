{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -fplugin GHC.Magic.Dict.Plugin #-}

module GHC.Magic.Dict.Errors where

import Data.Void (Void)
import GHC.Magic.Dict.Compat
import GHC.Magic.Dict.Defs

badClass1 :: Int -> Int
badClass1 = withDict @(BadClass1 Int) (id @Int) neg

badClass2 :: Int -> Int -> Bool
badClass2 = withDict @(BadClass2 Int) ((==) @Int) badeq2
