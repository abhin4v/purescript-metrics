module System.Metrics.Timer
  ( Timer
  , Summary(..)
  , new
  , update
  , time
  , fifteenMinuteRate
  , fiveMinuteRate
  , oneMinuteRate
  , meanRate
  , clear
  , percentiles
  , mean
  , stdDev
  , min
  , max
  , sum
  , count
  , read
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Ref (REF)
import Data.DateTime.Instant (unInstant)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Time.Duration (class Duration, Milliseconds(..), fromDuration)
import System.Metrics.Histogram (Histogram)
import System.Metrics.Histogram as Histogram
import System.Metrics.Meter (Meter)
import System.Metrics.Meter as Meter

-- | A timer is a histogram of the duration of a type of event and a meter of the rate of its occurrence.
newtype Timer = Timer { meter :: Meter, histogram :: Histogram }

-- | Creates a new timer.
new :: forall eff. Eff (ref :: REF | eff) Timer
new = Timer <$> ({ meter: _, histogram: _ }
                 <$> Meter.new
                 <*> Histogram.newWithExponentialDecaySampling 1028 0.015)

-- | Updates the timer with the duration of a new event.
update :: forall a eff. Duration a => Timer -> a -> Eff (ref :: REF | eff) Unit
update (Timer { meter, histogram }) d = do
  let (Milliseconds ms) = fromDuration d
  Histogram.update histogram ms
  Meter.mark meter

-- | Runs and times the given function, updates the given timer with the run duration, and returns
-- | the return value of the function.
time :: forall a eff. Timer -> Aff (ref :: REF, now :: NOW | eff) a -> Aff (ref :: REF, now :: NOW | eff) a
time timer f = do
  start <- liftEff now
  r <- f
  end <- liftEff now
  liftEff $ update timer (unInstant end - unInstant start)
  pure r

-- | Returns the fifteen minute moving average rate of the events recorded by the timer.
fifteenMinuteRate :: forall eff. Timer -> Eff (ref :: REF | eff) Number
fifteenMinuteRate (Timer { meter }) = Meter.fifteenMinuteRate meter

-- | Returns the five minute moving average rate of the events recorded by the timer.
fiveMinuteRate :: forall eff. Timer -> Eff (ref :: REF | eff) Number
fiveMinuteRate (Timer { meter }) = Meter.fiveMinuteRate meter

-- | Returns the one minute moving average rate of the events recorded by the timer.
oneMinuteRate :: forall eff. Timer -> Eff (ref :: REF | eff) Number
oneMinuteRate (Timer { meter }) = Meter.oneMinuteRate meter

-- | Returns the mean rate of the events recorded by the timer.
meanRate :: forall eff. Timer -> Eff (ref :: REF | eff) Number
meanRate (Timer { meter }) = Meter.meanRate meter

-- | Clears the timer.
clear :: forall eff. Timer -> Eff (ref :: REF | eff) Unit
clear (Timer { histogram }) = Histogram.clear histogram

-- | Returns the percentiles of the measurements in the timer for the given percentiles array.
percentiles :: forall eff. Timer -> Array Number -> Eff (ref :: REF | eff) (Maybe (Array Number))
percentiles (Timer { histogram }) = Histogram.percentiles histogram

-- | Returns the mean of the measurements in the timer.
mean :: forall eff. Timer -> Eff (ref :: REF | eff) (Maybe Number)
mean (Timer { histogram }) = Histogram.mean histogram

-- | Returns the standard deviation of the measurements in the timer.
stdDev :: forall eff. Timer -> Eff (ref :: REF | eff) (Maybe Number)
stdDev (Timer { histogram }) = Histogram.stdDev histogram

-- | Returns the minimum of the measurements in the timer.
min :: forall eff. Timer -> Eff (ref :: REF | eff) (Maybe Number)
min (Timer { histogram }) = Histogram.min histogram

-- | Returns the maximum of the measurements in the timer.
max :: forall eff. Timer -> Eff (ref :: REF | eff) (Maybe Number)
max (Timer { histogram }) = Histogram.max histogram

-- | Returns the sum of the measurements in the timer.
sum :: forall eff. Timer -> Eff (ref :: REF | eff) (Maybe Number)
sum (Timer { histogram }) = Histogram.sum histogram

-- | Returns the count of the measurements in the timer.
count :: forall eff. Timer -> Eff (ref :: REF | eff) Int
count (Timer { histogram }) = Histogram.count histogram

-- | Summary of the measurements in the meter. Contains:
-- |
-- | - duration: the summary of the underlying event duration histogram.
-- | - rate: the summary of the underlying event rate meter.
newtype Summary = Summary { duration :: Histogram.Summary, rate :: Meter.Summary }

derive instance eqTSummary :: Eq Summary
derive instance genericTSummary :: Generic Summary _
instance showTSummary :: Show Summary where
  show = genericShow
instance encodeTSummary :: Encode Summary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

-- | Returns the summary of the measurements in the timer.
read :: forall eff. Timer -> Eff (ref :: REF | eff) Summary
read (Timer { meter, histogram }) =
  Summary <$> ({ duration: _, rate: _} <$> Histogram.read histogram <*> Meter.read meter)
