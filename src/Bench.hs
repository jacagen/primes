module Bench (bench) where

import Control.DeepSeq (NFData, deepseq)
import Data.Time.Clock.System (SystemTime (..), getSystemTime)

systemTimeToSeconds :: SystemTime -> Double
systemTimeToSeconds (MkSystemTime seconds nanoseconds) =
  fromIntegral seconds + (fromIntegral nanoseconds / 1e9)

bench :: (NFData a) => a -> IO Double
bench f = do
  start <- getSystemTime
  end <- f `deepseq` getSystemTime
  return (systemTimeToSeconds end - systemTimeToSeconds start)
