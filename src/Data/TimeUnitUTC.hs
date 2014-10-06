module Data.TimeUnitUTC 
   ( timeUnitToDiffTime
   , diffTimeToTimeUnit
   ) where

import Data.Time.Units
import Data.Time.Clock

timeUnitToDiffTime :: TimeUnit a => a -> NominalDiffTime
timeUnitToDiffTime t = fromInteger $ (toMicroseconds t) `div` microsecondsPerSecond

diffTimeToTimeUnit :: NominalDiffTime -> Second
diffTimeToTimeUnit t = fromMicroseconds . floor $ t * fromInteger microsecondsPerSecond

microsecondsPerSecond :: Num a => a
microsecondsPerSecond = 10 ^ (6 :: Integer)
