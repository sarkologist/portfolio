{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}

module Distance where

import Segment

import Data.Time.Clock
import Data.VectorSpace

import Data.Function
import Control.Monad.State.Class
import Pipes
import Pipes.Lift

type V3 = (Double, Double, Double)

-- returns the value of the parameter of the segment corresponding to the closest point
closestPointTo :: V3 -> Segment -> Scalar V3
closestPointTo p (Segment v w) =
  if squaredLength == 0
  then 0
  else let t = ((p ^-^ v) <.> (w ^-^ v)) / squaredLength
           tClamped = max 0 (min 1 t)
       in tClamped
    where
      squaredLength = magnitudeSq (v ^-^ w)

data Closeness =
  Closeness {
    closenessTime :: UTCTime
  , closenessPoint :: V3
  , closenessDistance :: Scalar V3
  } deriving Show

closestDistances :: Monad m => V3 -> Pipe SegmentEvent Closeness m ()
closestDistances p = evalStateP Nothing loop
  where loop = do
          se@SegmentEvent{..} <- await
          let tClosest = closestPointTo p seSegment
              Segment v w = seSegment
              closest = lerp v w tClosest
              thisClose = Closeness { closenessTime = time tClosest seTimeStart seDuration
                                    , closenessPoint = closest
                                    , closenessDistance = magnitude (closest ^-^ p)
                                    }

          closestSoFar <- maybe thisClose (closer thisClose) <$> get
          put (Just closestSoFar)
          yield closestSoFar

          loop

        -- assumptions
        -- 1. constant speed along segment
        -- 2. t is in the interval [0,1]
        time t time dur = addUTCTime (realToFrac t * dur) time -- here we are multiplying a scalar in [0,1] into seconds of `NominalDiffTime`

        closer x y = if ((<=) `on` closenessDistance) x y
                     then x
                     else y
