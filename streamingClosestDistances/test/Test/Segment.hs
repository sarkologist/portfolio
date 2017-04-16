module Test.Segment where

import Segment
import Parse

import Pipes
import qualified Pipes.Prelude as PP
import Control.Applicative
import Data.Function

import Data.Time.Clock
import Data.VectorSpace

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

segmentTests = testGroup "Segment tests"
  [ testCase "one point" $ do
      let events = take 2 . getZipList $
                CoordEvent
               <$> ZipList (iterate (addUTCTime 21) t0)
               <*> pure zeroV
      toSegmentEvents events @?= [ SegmentEvent t0 0 (Segment zeroV zeroV) ]

  , SC.testProperty "sequences of points" $ \n ->
      let coords = getZipList $ CoordEvent <$> ZipList (times 21 t0) <*> ZipList positions
          segments = getZipList $ SegmentEvent
                  <$> ZipList (times 21 t0)
                  <*> pure 0
                  <*> (Segment <$> ZipList positions <*> ZipList positions)

      in ((==) `on` take (n+1)) (toSegmentEvents coords) segments

  , testCase "one segment" $ do
      let events = getZipList $
                CoordEvent
               <$> ZipList (iterate (addUTCTime 20) t0)
               <*> ZipList [ zeroV, (1,1,1) ]
      toSegmentEvents events @?= [ SegmentEvent t0 20 (Segment zeroV (1,1,1)) ]

  , SC.testProperty "point then segments" $ \n ->
      let coords = getZipList $ CoordEvent <$> ZipList (t0 : (times 20 (addUTCTime 30 t0))) <*> ZipList positions
          point = SegmentEvent t0 0 (Segment zeroV zeroV)
          segments = getZipList $
                    SegmentEvent
                  <$> ZipList (times 20 (addUTCTime 30 t0))
                  <*> pure 20
                  <*> (Segment <$> ZipList (tail positions) <*> ZipList (tail (tail positions)))
      in ((==) `on` take (n+1)) (toSegmentEvents coords) (point:segments)

  , SC.testProperty "segments then point" $ \n -> n > 0 SC.==>
      let coords = getZipList $ CoordEvent <$> ZipList ts <*> ZipList positions
          segments = getZipList $
                    SegmentEvent
                  <$> ZipList ts
                  <*> pure 20
                  <*> (Segment <$> ZipList positions <*> ZipList (tail positions))
          point = SegmentEvent finalTime 0 (Segment finalPosition finalPosition)

          ts = take (n+1) (times 20 t0) ++ [ finalTime ]
          finalTime = addUTCTime (20 * realToFrac n + 30) t0
          finalPosition = positions !! (n+1)

      in toSegmentEvents coords == (take n segments ++ [point])

  , SC.testProperty "sequences of segments" $ \n ->
      let coords = getZipList $ CoordEvent <$> ZipList (times 20 t0) <*> ZipList positions
          segments = getZipList $
              SegmentEvent
            <$> ZipList (times 20 t0)
            <*> pure 20
            <*> (Segment <$> ZipList positions <*> ZipList (tail positions))
      in ((==) `on` take (n+1)) (toSegmentEvents coords) segments
  ]

toSegmentEvents :: [ CoordEvent ] -> [ SegmentEvent ]
toSegmentEvents events = PP.toList (each events >-> segments)

t0 = UTCTime (toEnum 0) (secondsToDiffTime 0)
times dt = iterate (addUTCTime dt)

positions = iterate (^+^ (1,1,1)) zeroV
