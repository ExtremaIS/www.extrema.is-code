module Demo1.IO.Test (tests) where

-- https://hackage.haskell.org/package/containers
import qualified Data.Map.Strict as Map

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit ((@=?), testCase)

-- https://hackage.haskell.org/package/tasty-quickcheck
import Test.Tasty.QuickCheck (testProperty)

-- (feature-flag-demo)
import qualified Demo1.FeatureFlag as FF
import qualified Demo1.IO

------------------------------------------------------------------------------

testFoo :: TestTree
testFoo = testGroup "foo"
    [ testCase "-ffSomeBug" $ 13 @=? Demo1.IO.foo False
    , testCase "+ffSomeBug" $ 42 @=? Demo1.IO.foo True
    ]

testBar :: TestTree
testBar = testProperty "bar" prop_equiv
  where
    prop_equiv :: Int -> Bool
    prop_equiv n = Demo1.IO.bar False n == Demo1.IO.bar True n

------------------------------------------------------------------------------

testRun :: TestTree
testRun = testGroup "run"
    [ testCase "-ffSomeBug -ffSomeBusinessLogic" $
        (26 @=?) =<< Demo1.IO.run (ffMap False False)
    , testCase "-ffSomeBug +ffSomeBusinessLogic" $
        (26 @=?) =<< Demo1.IO.run (ffMap False True)
    , testCase "+ffSomeBug -ffSomeBusinessLogic" $
        (84 @=?) =<< Demo1.IO.run (ffMap True False)
    , testCase "+ffSomeBug +ffSomeBusinessLogic" $
        (84 @=?) =<< Demo1.IO.run (ffMap True True)
    ]
  where
    ffMap :: Bool -> Bool -> FF.FeatureFlagMap
    ffMap ffSomeBug ffSomeBusinessLogic = Map.fromList
      [ (FF.Fix_202407_SomeBug_42, ffSomeBug)
      , (FF.Ref_202407_SomeBusinessLogic, ffSomeBusinessLogic)
      ]

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Demo1.IO"
    [ testFoo
    , testBar
    , testRun
    ]
