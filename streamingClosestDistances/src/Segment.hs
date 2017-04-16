module Segment where

import Parse

import Pipes
import Data.Function

import Data.Time.Clock

trajectorySeparation :: NominalDiffTime
trajectorySeparation = 20

data Segment = Segment V3 V3
  deriving (Eq, Show)

data SegmentEvent =
  SegmentEvent {
    seTimeStart :: UTCTime
  , seDuration :: NominalDiffTime
  , seSegment :: Segment
  } deriving (Eq, Show)

segments :: Monad m => Pipe CoordEvent SegmentEvent m ()
segments = loop False Nothing
  where
    loop consumed mOne = do
      one <- maybe await pure mOne
      two <- await

      if closeEnough one two
      then segment one two >> loop True (Just two)
      else if consumed
          then point two >> loop True Nothing
          else point one >> loop False (Just two)

    closeEnough = occurWithin trajectorySeparation `on` eTime

    point p = segment p p

    segment start end =
      yield SegmentEvent {
              seTimeStart = eTime start
            , seDuration = diffUTCTime (eTime end) (eTime start)
            , seSegment = Segment (eCoord start) (eCoord end)
            }

occurWithin :: NominalDiffTime -> UTCTime -> UTCTime -> Bool
occurWithin dt = (fmap.fmap) (<=dt) (flip diffUTCTime)

