{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE FlexibleInstances #-}

module Parse where

import Pipes
import qualified Pipes.Prelude as PP
import qualified Pipes.ByteString    as PB
import Pipes.Csv
import System.IO

import Control.Applicative

import Data.Time.Format
import Data.Time.Clock

type V3 = (Double, Double, Double)

type ParseCoordEvent m = Producer PB.ByteString m () -> Producer CoordEvent m ()

events :: Monad m => ParseCoordEvent m
events bs = decodeByName bs >-> PP.concat

data CoordEvent =
  CoordEvent {
    eTime :: UTCTime
  , eCoord :: V3
  } deriving (Show)

instance FromNamedRecord CoordEvent where
  parseNamedRecord p =
    CoordEvent <$> time
               <*> coord
    where
      time = parseISODateString =<< p .: "timestamp"

      coord = (,,) <$> p .: "x"
                   <*> p .: "y"
                   <*> p .: "z"

      parseISODateString :: (Monad m, ParseTime t) => String -> m t
      parseISODateString = parseTimeM True defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S%QZ")
