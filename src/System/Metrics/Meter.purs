module System.Metrics.Meter
  ( Meter
  , Summary(..)
  , new
  , markN
  , mark
  , fifteenMinuteRate
  , fiveMinuteRate
  , oneMinuteRate
  , meanRate
  , count
  , read
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, genericEncode)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Prelude

-- | A mutable metric which measures the rate at which a set of events occur.
foreign import data Meter :: Type
foreign import _new :: forall eff. Eff (ref :: REF | eff) Meter
foreign import _markN :: forall eff. Fn2 Meter Int (Eff (ref :: REF | eff) Unit)

-- | Returns the fifteen minute moving-average rate of the events recorded by the meter.
foreign import fifteenMinuteRate :: forall eff. Meter -> Eff (ref :: REF | eff) Number

-- | Returns the five minute moving-average rate of the events recorded by the meter.
foreign import fiveMinuteRate :: forall eff. Meter -> Eff (ref :: REF | eff) Number

-- | Returns the one minute moving-average rate of the events recorded by the meter.
foreign import oneMinuteRate :: forall eff. Meter -> Eff (ref :: REF | eff) Number

-- | Returns the mean rate of the events recorded by the meter.
foreign import meanRate :: forall eff. Meter -> Eff (ref :: REF | eff) Number

-- | Returns the count of the events recorded by the meter.
foreign import count :: forall eff. Meter -> Eff (ref :: REF | eff) Int

-- | Create a new meter.
new :: forall eff. Eff (ref :: REF | eff) Meter
new = _new

-- | Marks the meter with occurrance of n events.
markN :: forall eff. Meter -> Int -> Eff (ref :: REF | eff) Unit
markN = runFn2 _markN

-- | Marks the meter with occurrance of one event.
mark :: forall eff. Meter -> Eff (ref :: REF | eff) Unit
mark m = markN m 1

-- | Summary of the measurements in the meter. Contains:
-- |
-- | - count: count
-- | - m1: one minute rate
-- | - m5: five minute rate
-- | - m15: fifteen minute rate
-- | - mean: mean rate
newtype Summary = Summary {
                    count :: Int
                  , m1 :: Number
                  , m5 :: Number
                  , m15 :: Number
                  , mean :: Number
                  }

derive instance eqMSummary :: Eq Summary
derive instance genericMSummary :: Generic Summary _
instance showMSummary :: Show Summary where
  show = genericShow
instance encodeMSummary :: Encode Summary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

-- | Returns the summary of the measurements in the meter.
read :: forall eff. Meter -> Eff (ref :: REF | eff) Summary
read m = Summary <$> ({  count: _
                      , m1: _
                      , m5: _
                      , m15: _
                      , mean: _
                      } <$> count m
                        <*> oneMinuteRate m
                        <*> fiveMinuteRate m
                        <*> fifteenMinuteRate m
                        <*> meanRate m)
