import Test.Distance
import Test.Segment

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests = testGroup "Unit tests"
  [ segmentTests
  , distanceTests
  ]
