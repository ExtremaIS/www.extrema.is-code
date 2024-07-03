{-# LANGUAGE DataKinds #-}

module Demo2.IO.Test (tests) where

-- https://hackage.haskell.org/package/dependent-map
import qualified Data.Dependent.Map as DMap

-- https://hackage.haskell.org/package/dependent-sum
import Data.Dependent.Sum ((==>))

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit ((@=?), testCase)

-- https://hackage.haskell.org/package/tasty-quickcheck
import Test.Tasty.QuickCheck (testProperty)

-- (feature-flag-demo)
import qualified Demo2.FeatureFlag as FF
import qualified Demo2.IO

------------------------------------------------------------------------------

testFoo :: TestTree
testFoo = testGroup "foo"
    [ testCase "-ffSomeBug" $ 13 @=? Demo2.IO.foo (ffSomeBug (Just False))
    , testCase "+ffSomeBug" $ 42 @=? Demo2.IO.foo (ffSomeBug (Just True))
    , testCase "!ffSomeBug" $ 42 @=? Demo2.IO.foo (ffSomeBug Nothing)
    ]
  where
    ffSomeBug :: Maybe Bool -> FF.FeatureFlagConfig FF.Fix_202407_SomeBug_42
    ffSomeBug = FF.FeatureFlagConfig

testBar :: TestTree
testBar = testProperty "bar" prop_equiv
  where
    prop_equiv :: Int -> Bool
    prop_equiv n =
      Demo2.IO.bar (ffSomeBusinessLogic False) n
        == Demo2.IO.bar (ffSomeBusinessLogic True) n

    ffSomeBusinessLogic
      :: Bool
      -> FF.FeatureFlagConfig FF.Ref_202407_SomeBusinessLogic
    ffSomeBusinessLogic = FF.FeatureFlagConfig . Just

------------------------------------------------------------------------------

testRun :: TestTree
testRun = testGroup "run"
    [ testCase "-ffSomeBug -ffSomeBusinessLogic" $
        (26 @=?) =<< Demo2.IO.run (ffMap False False)
    , testCase "-ffSomeBug +ffSomeBusinessLogic" $
        (26 @=?) =<< Demo2.IO.run (ffMap False True)
    , testCase "+ffSomeBug -ffSomeBusinessLogic" $
        (84 @=?) =<< Demo2.IO.run (ffMap True False)
    , testCase "+ffSomeBug +ffSomeBusinessLogic" $
        (84 @=?) =<< Demo2.IO.run (ffMap True True)
    ]
  where
    ffMap :: Bool -> Bool -> FF.FeatureFlagMap
    ffMap ffSomeBug ffSomeBusinessLogic = DMap.fromList
      [ FF.Fix_202407_SomeBug_42 ==> ffSomeBug
      , FF.Ref_202407_SomeBusinessLogic ==> ffSomeBusinessLogic
      ]

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Demo2.IO"
    [ testFoo
    , testBar
    , testRun
    ]
