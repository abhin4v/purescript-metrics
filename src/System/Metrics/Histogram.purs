module System.Metrics.Histogram
  ( Histogram
  , Summary
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

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Data.Array (index)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Prelude

foreign import data Histogram :: Type
foreign import _newWithExponentialDecaySampling ::
  forall eff. Fn2 Int Number (Eff (ref :: REF | eff) Histogram)
foreign import newWithUniformSampling ::
  forall eff. Int -> Eff (ref :: REF | eff) Histogram
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
foreign import count :: forall eff. Histogram -> Eff (ref :: REF | eff) Int

newWithExponentialDecaySampling :: forall eff. Int -> Number -> Eff (ref :: REF | eff) Histogram
newWithExponentialDecaySampling = runFn2 _newWithExponentialDecaySampling

update :: forall eff. Histogram -> Number -> Eff (ref :: REF | eff) Unit
update = runFn2 _update

percentiles :: forall eff. Histogram -> Array Number -> Eff (ref :: REF | eff) (Maybe (Array Number))
percentiles h ptiles = toMaybe <$> runFn2 _percentiles h ptiles

variance :: forall eff. Histogram -> Eff (ref :: REF | eff) (Maybe Number)
variance h = toMaybe <$> _variance h

mean :: forall eff. Histogram -> Eff (ref :: REF | eff) (Maybe Number)
mean h = toMaybe <$> _mean h

stdDev :: forall eff. Histogram -> Eff (ref :: REF | eff) (Maybe Number)
stdDev h = toMaybe <$> _stdDev h

min :: forall eff. Histogram -> Eff (ref :: REF | eff) (Maybe Number)
min h = toMaybe <$> _min h

max :: forall eff. Histogram -> Eff (ref :: REF | eff) (Maybe Number)
max h = toMaybe <$> _max h

sum :: forall eff. Histogram -> Eff (ref :: REF | eff) (Maybe Number)
sum h = toMaybe <$> _sum h

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

derive instance eqSummary :: Eq Summary
derive instance genericSummary :: Generic Summary _
instance showSummary :: Show Summary where
  show = genericShow

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
