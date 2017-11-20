module System.Metrics.Histogram
  ( Histogram
  , Summary(..)
  , newWithExponentialDecaySampling
  , newWithUniformSampling
  , clear
  , update
  , percentiles
  , variance
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
import Data.Array (index)
import Data.Foreign.Class (class Encode, encode)
import Data.Foreign.Generic (defaultOptions, genericEncode)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)

-- | A mutable distribution of values in a stream of data.
foreign import data Histogram :: Type
foreign import _newWithExponentialDecaySampling ::
  forall eff. Fn2 Int Number (Eff (ref :: REF | eff) Histogram)

-- | Creates a new histogram with uniform sampling with the given size.
foreign import newWithUniformSampling ::
  forall eff. Int -> Eff (ref :: REF | eff) Histogram

-- | Clears the histogram.
foreign import clear :: forall eff. Histogram -> Eff (ref :: REF | eff) Unit
foreign import _update :: forall eff. Fn2 Histogram Number (Eff (ref :: REF | eff) Unit)
foreign import _percentiles
  :: forall eff. Fn2 Histogram (Array Number) (Eff (ref :: REF | eff) (Nullable (Array Number)))
foreign import _variance :: forall eff. Histogram -> Eff (ref :: REF | eff) (Nullable Number)
foreign import _mean :: forall eff. Histogram -> Eff (ref :: REF | eff) (Nullable Number)
foreign import _stdDev :: forall eff. Histogram -> Eff (ref :: REF | eff) (Nullable Number)
foreign import _min :: forall eff. Histogram -> Eff (ref :: REF | eff) (Nullable Number)
foreign import _max :: forall eff. Histogram -> Eff (ref :: REF | eff) (Nullable Number)
foreign import _sum :: forall eff. Histogram -> Eff (ref :: REF | eff) (Nullable Number)

-- | Returns the count of measurements in the histogram.
foreign import count :: forall eff. Histogram -> Eff (ref :: REF | eff) Int

-- | Creates a new histogram with exponential decay sampling with given size and alpha.
newWithExponentialDecaySampling :: forall eff. Int -> Number -> Eff (ref :: REF | eff) Histogram
newWithExponentialDecaySampling = runFn2 _newWithExponentialDecaySampling

-- | Updates the histogram with the given measurement.
update :: forall eff. Histogram -> Number -> Eff (ref :: REF | eff) Unit
update = runFn2 _update

-- | Returns the percentiles of the measurements in the histogram for the given percentiles array.
-- | Returns 'Nothing' if there are no measurements.
percentiles :: forall eff. Histogram -> Array Number -> Eff (ref :: REF | eff) (Maybe (Array Number))
percentiles h ptiles = toMaybe <$> runFn2 _percentiles h ptiles

-- | Returns the variance of the measurements in the histogram.
-- | Returns 'Nothing' if there are no measurements.
variance :: forall eff. Histogram -> Eff (ref :: REF | eff) (Maybe Number)
variance h = toMaybe <$> _variance h

-- | Returns the mean of the measurements in the histogram.
-- | Returns 'Nothing' if there are no measurements.
mean :: forall eff. Histogram -> Eff (ref :: REF | eff) (Maybe Number)
mean h = toMaybe <$> _mean h

-- | Returns the standard deviation of the measurements in the histogram.
-- | Returns 'Nothing' if there are no measurements.
stdDev :: forall eff. Histogram -> Eff (ref :: REF | eff) (Maybe Number)
stdDev h = toMaybe <$> _stdDev h

-- | Returns the minimum of the measurements in the histogram.
-- | Returns 'Nothing' if there are no measurements.
min :: forall eff. Histogram -> Eff (ref :: REF | eff) (Maybe Number)
min h = toMaybe <$> _min h

-- | Returns the maximum of the measurements in the histogram.
-- | Returns 'Nothing' if there are no measurements.
max :: forall eff. Histogram -> Eff (ref :: REF | eff) (Maybe Number)
max h = toMaybe <$> _max h

-- | Returns the sum of the measurements in the histogram.
-- | Returns 'Nothing' if there are no measurements.
sum :: forall eff. Histogram -> Eff (ref :: REF | eff) (Maybe Number)
sum h = toMaybe <$> _sum h

-- | Summary of the distribution of the measurements in the histogram.
-- | Contains:
-- |
-- | - min: minimum
-- | - max: maximum
-- | - sum: sum
-- | - variance: variance
-- | - mean: mean
-- | - stdDev: standard deviation
-- | - count: count
-- | - median: median
-- | - p75: 75th percentile
-- | - p95: 95th percentile
-- | - p99: 99th percentile
-- | - p999: 99.9th percentile
newtype Summary = Summary {
    min      :: Maybe Number
  , max      :: Maybe Number
  , sum      :: Maybe Number
  , variance :: Maybe Number
  , mean     :: Maybe Number
  , stdDev   :: Maybe Number
  , count    :: Int
  , median   :: Maybe Number
  , p75      :: Maybe Number
  , p95      :: Maybe Number
  , p99      :: Maybe Number
  , p999     :: Maybe Number
}

derive instance eqHSummary :: Eq Summary
derive instance genericHSummary :: Generic Summary _
instance showHSummary :: Show Summary where
  show = genericShow
instance encodeHSummary :: Encode Summary where
  encode = fromSummary >>> encode

newtype Summary' = Summary' {
    min      :: NullOrUndefined Number
  , max      :: NullOrUndefined Number
  , sum      :: NullOrUndefined Number
  , variance :: NullOrUndefined Number
  , mean     :: NullOrUndefined Number
  , stdDev   :: NullOrUndefined Number
  , count    :: Int
  , median   :: NullOrUndefined Number
  , p75      :: NullOrUndefined Number
  , p95      :: NullOrUndefined Number
  , p99      :: NullOrUndefined Number
  , p999     :: NullOrUndefined Number
}

fromSummary :: Summary -> Summary'
fromSummary (Summary { min , max , sum , variance , mean , stdDev , count , median , p75 , p95 , p99 , p999 }) =
  Summary' { min: NullOrUndefined min
           , max: NullOrUndefined max
           , sum: NullOrUndefined sum
           , variance: NullOrUndefined variance
           , mean: NullOrUndefined mean
           , stdDev: NullOrUndefined stdDev
           , count: count
           , median: NullOrUndefined median
           , p75: NullOrUndefined p75
           , p95: NullOrUndefined p95
           , p99: NullOrUndefined p99
           , p999: NullOrUndefined p999 }

derive instance genericHSummary' :: Generic Summary' _
instance encodeHSummary' :: Encode Summary' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

-- | Returns the summary of the measurements in the histogram.
read :: forall eff. Histogram -> Eff (ref :: REF | eff) Summary
read h = do
  ptiles <- percentiles h [0.5, 0.75, 0.95, 0.99, 0.999]
  Summary <$> ({ min: _
               , max: _
               , sum: _
               , variance: _
               , mean: _
               , stdDev: _
               , count: _
               , median: ptiles >>= flip index 0
               , p75: ptiles >>= flip index 1
               , p95: ptiles >>= flip index 2
               , p99: ptiles >>= flip index 3
               , p999: ptiles >>= flip index 4
               } <$> min h
                 <*> max h
                 <*> sum h
                 <*> variance h
                 <*> mean h
                 <*> stdDev h
                 <*> count h)
