{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE StandaloneDeriving #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Distance where

import Distance
import Segment

import Data.VectorSpace

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Assertions

distanceTests = testGroup "Distance tests"
  [ testCase "point incident to degenerate segment" $ do
      let zero = (0,0,0)
      closestPointTo zero (Segment zero zero)  @?= 0

  , testCase "point incident distance to segment endpoint" $ do
      let zero = (0,0,0)
          one = (1,1,1)
      closestPointTo zero (Segment zero one)  @?= 0
      closestPointTo zero (Segment one zero)  @?= 1
      closestPointTo one (Segment zero one)  @?= 1
      closestPointTo one (Segment one zero)  @?= 0

  , propertyTests
  ]

propertyTests = testGroup "closestPointTo function"
  [ closestPointTo_parameters
  , closestPointTo_distance
  ]

closestPointTo_parameters = testGroup "segment parameter"
  [ testProperty "is in interval [0,1]" $ \ p seg ->
      let t = closestPointTo p seg
      in 0 <=? t .&&. t <=? 1

  , testProperty "when point is on segment" $ \ (SegmentParameter t) seg@(Segment v w) ->
      let p = lerp v w t
          t' = closestPointTo p seg
      in counterexample (show p) $
           not (isDegenerate seg) ==>
             t' ~==? t
  ]

closestPointTo_distance = testGroup "distance"
  [ testProperty "to point on segment is zero" $ \ (SegmentParameter t) seg@(Segment v w) ->
      let p = lerp v w t
          t' = closestPointTo p seg
          distance = magnitude (lerp v w t' ^-^ p)
      in counterexample (show p) $
           withinTolerance 1e-10 distance 0
  ]

instance Arbitrary Segment where
  arbitrary = Segment <$> arbitrary <*> arbitrary

isDegenerate :: Segment -> Bool
isDegenerate (Segment v w) = v == w

withinTolerance :: (Show a, Num a, Ord a) => a -> a -> a -> Result
withinTolerance tolerance actual expect =
  abs (expect - actual) <? tolerance

newtype SegmentParameter = SegmentParameter (Scalar V3) deriving (Show)

instance Arbitrary SegmentParameter where
  arbitrary = SegmentParameter <$> choose (0,1)
