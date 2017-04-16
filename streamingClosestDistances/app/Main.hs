module Main where

import Parse
import Segment
import Distance

import Pipes
import qualified Pipes.Prelude as PP
import qualified Pipes.ByteString as PB

import Options.Applicative
import Data.Semigroup ((<>))

import System.Environment
import System.IO
import Text.Groom

main :: IO ()
main = do
  Args point <- execParser opts
  void $ go point
  where
    opts = info (args <**> helper)
      ( fullDesc
     <> progDesc "Compute the closest point that user UID has ever been to POINT"
     <> header "compute closest point" )

go point =
  let pipeline = events PB.stdin
             >-> segments
             >-> closestDistances point

      pprint = for pipeline (liftIO . putStrLn . groom)
  in runEffect pprint


data Args = Args { point :: (Double, Double, Double) }

args :: Parser Args
args = Args
      <$> option auto
          ( long "closest-to"
         <> short 'c'
         <> metavar "POINT"
         <> help "find closest distances to POINT" )
