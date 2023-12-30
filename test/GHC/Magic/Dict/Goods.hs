{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -dcore-lint -ddump-tc-trace #-}
{-# OPTIONS_GHC -fplugin GHC.Magic.Dict.Plugin #-}

module GHC.Magic.Dict.Goods where

import Data.Void (Void)
import GHC.Magic.Dict.Compat
import GHC.Magic.Dict.Defs

eqEqInt :: Int -> Int -> Bool
eqEqInt = withDict @(MyEq Int) ((==) @Int) eq
