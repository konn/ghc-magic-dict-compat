{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module GHC.Magic.Dict.CompatSpec (test_fails, test_successes) where

import Control.Exception
import Data.Char (chr, ord)
import Data.Function (on)
import GHC.Magic.Dict.Defs
import GHC.Magic.Dict.Errors
import GHC.Magic.Dict.Goods
import qualified Test.Falsify.Generator as F
import Test.Falsify.Predicate ((.$))
import qualified Test.Falsify.Predicate as P
import qualified Test.Falsify.Range as F
import Test.Tasty
import Test.Tasty.Falsify (testProperty)
import qualified Test.Tasty.Falsify as F
import Test.Tasty.HUnit

newtype OpaqueInt = OpaqueInt {getInt :: Int}

test_fails :: TestTree
test_fails =
  testGroup
    "Must type error"
    [ testCase "withEqFail" $ expectTypeError $ withEqFail @OpaqueInt ((==) `on` getInt) ((==) @OpaqueInt)
    , testCase "badClass1 (unsaturated)" $ expectTypeError badClass1
    , testCase "badClass1" $ expectTypeError (badClass1 42)
    , testCase "badClass2 (unsaturated)" $ expectTypeError badClass2
    , testCase "badClass2" $ expectTypeError (badClass2 42 34)
    , testCase "badVoid" $ expectTypeError badVoid
    ]

expectTypeError :: a -> Assertion
expectTypeError ans =
  try (evaluate ans) >>= \case
    Left (fromException -> Just TypeError {}) -> pure ()
    Left e -> assertFailure $ "Expected TypeError, got " <> show e
    Right _ -> assertFailure "Expected TypeError, got no exception"

test_successes :: TestTree
test_successes =
  testGroup
    "Must pass"
    [ testProperty "eq2MyEqInt === (==) @Int" $ do
        i <- F.gen $ F.int $ F.between (minBound, maxBound)
        j <- F.gen $ F.int $ F.between (minBound, maxBound)
        F.assert $ P.expect (i == j) .$ ("result", eq2MyEqInt i j)
    , testProperty "neq2MyEqInt === (/=) @Int" $ do
        i <- F.gen $ F.int $ F.between (minBound, maxBound)
        j <- F.gen $ F.int $ F.between (minBound, maxBound)
        F.assert $ P.expect (i /= j) .$ ("result", neq2MyEqInt i j)
    , testProperty "eq2MyEqPoly @Bool === (==) @Bool" $ do
        i <- F.gen $ F.bool False
        j <- F.gen $ F.bool False
        F.assert $ P.expect (i == j) .$ ("result", eq2MyEqPoly i j)
    , testProperty "eq2MyEqPoly @Bool === (/=) @Bool" $ do
        i <- F.gen $ F.bool False
        j <- F.gen $ F.bool False
        F.assert $ P.expect (i /= j) .$ ("result", neq2MyEqPoly i j)
    , testProperty "unRelatedBy (\\i c -> i == ord c) === (i /= ord c)" $ do
        i <- F.gen $ F.int $ F.between (0, 255)
        j <- F.gen $ chr <$> F.int (F.between (0, 255))
        F.assert $ P.expect (i /= ord j) .$ ("result", unRelatedBy (\n c -> n == ord c) i j)
    , testProperty "unRelatedBy @(Bool, Bool) @Bool r === fmap not . r" $ do
        F.Fn2 rel <- F.gen $ F.fun $ F.bool False
        p <- F.gen $ F.bool False
        q <- F.gen $ F.bool False
        F.assert $
          P.expect (not $ rel p q)
            .$ ("result", unRelatedBy rel p q)
    , testProperty "give i given == i" $ do
        i <- F.gen $ F.int $ (-128, 128) `F.withOrigin` 0
        F.assert $ P.expect i .$ ("result", give i given)
    , testProperty "give i (give j given) == i" $ do
        i <-
          F.genWith (Just . ("i = " <>) . show) $
            F.int $
              (-128, 128) `F.withOrigin` 0
        j <-
          F.genWith (Just . ("j = " <>) . show) $
            F.int $
              (-128, 128) `F.withOrigin` 0
        F.assert $ P.expect i .$ ("result", give i (give j given))
    , testProperty "give i (given, give j given) == (i, i)" $ do
        i <-
          F.genWith (Just . ("i = " <>) . show) $
            F.int $
              (-128, 128) `F.withOrigin` 0
        j <-
          F.genWith (Just . ("j = " <>) . show) $
            F.int $
              (-128, 128) `F.withOrigin` 0
        F.assert $ P.expect (i, i) .$ ("result", give i (given, give j given))
    , testProperty "(give i given, give j given) == (i, j)" $ do
        i <-
          F.genWith (Just . ("i = " <>) . show) $
            F.int $
              (-128, 128) `F.withOrigin` 0
        j <-
          F.genWith (Just . ("j = " <>) . show) $
            F.int $
              (-128, 128) `F.withOrigin` 0
        F.assert $ P.expect (i, j) .$ ("result", (give i given, give j given))
    ]
