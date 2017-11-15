module System.Metrics.Timer
  ( Timer
  , Summary
  , new
  , update
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

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Time.Duration (class Duration, Milliseconds(..), fromDuration)
import System.Metrics.Histogram (Histogram)
import System.Metrics.Histogram as Histogram
import System.Metrics.Meter (Meter)
import System.Metrics.Meter as Meter

newtype Timer = Timer { meter :: Meter, histogram :: Histogram }

new :: forall eff. Eff (ref :: REF | eff) Timer
new = Timer <$> ({ meter: _, histogram: _ }
                 <$> Meter.new
                 <*> Histogram.newWithExponentialDecaySampling 1028 0.015)

update :: forall a eff. Duration a => Timer -> a -> Eff (ref :: REF | eff) Unit
update (Timer { meter, histogram }) d = do
  let (Milliseconds ms) = fromDuration d
  Histogram.update histogram ms
  Meter.mark meter

fifteenMinuteRate :: forall eff. Timer -> Eff (ref :: REF | eff) Number
fifteenMinuteRate (Timer { meter }) = Meter.fifteenMinuteRate meter

fiveMinuteRate :: forall eff. Timer -> Eff (ref :: REF | eff) Number
fiveMinuteRate (Timer { meter }) = Meter.fiveMinuteRate meter

oneMinuteRate :: forall eff. Timer -> Eff (ref :: REF | eff) Number
oneMinuteRate (Timer { meter }) = Meter.oneMinuteRate meter

meanRate :: forall eff. Timer -> Eff (ref :: REF | eff) Number
meanRate (Timer { meter }) = Meter.meanRate meter

clear :: forall eff. Timer -> Eff (ref :: REF | eff) Unit
clear (Timer { histogram }) = Histogram.clear histogram

percentiles :: forall eff. Timer -> Array Number -> Eff (ref :: REF | eff) (Maybe (Array Number))
percentiles (Timer { histogram }) = Histogram.percentiles histogram

mean :: forall eff. Timer -> Eff (ref :: REF | eff) (Maybe Number)
mean (Timer { histogram }) = Histogram.mean histogram

stdDev :: forall eff. Timer -> Eff (ref :: REF | eff) (Maybe Number)
stdDev (Timer { histogram }) = Histogram.stdDev histogram

min :: forall eff. Timer -> Eff (ref :: REF | eff) (Maybe Number)
min (Timer { histogram }) = Histogram.min histogram

max :: forall eff. Timer -> Eff (ref :: REF | eff) (Maybe Number)
max (Timer { histogram }) = Histogram.max histogram

sum :: forall eff. Timer -> Eff (ref :: REF | eff) (Maybe Number)
sum (Timer { histogram }) = Histogram.sum histogram

count :: forall eff. Timer -> Eff (ref :: REF | eff) Int
count (Timer { histogram }) = Histogram.count histogram

newtype Summary = Summary { duration :: Histogram.Summary, rate :: Meter.Summary }

derive instance eqSummary :: Eq Summary
derive instance genericSummary :: Generic Summary _
instance showSummary :: Show Summary where
  show = genericShow

read :: forall eff. Timer -> Eff (ref :: REF | eff) Summary
read (Timer { meter, histogram }) =
  Summary <$> ({ duration: _, rate: _} <$> Histogram.read histogram <*> Meter.read meter)
