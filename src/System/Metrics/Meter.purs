module System.Metrics.Meter
  ( Meter
  , Summary
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
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Prelude (class Eq, class Show, Unit, (<$>), (<*>))

foreign import data Meter :: Type
foreign import _new :: forall eff. Eff (ref :: REF | eff) Meter
foreign import _markN :: forall eff. Fn2 Meter Int (Eff (ref :: REF | eff) Unit)
foreign import fifteenMinuteRate :: forall eff. Meter -> Eff (ref :: REF | eff) Number
foreign import fiveMinuteRate :: forall eff. Meter -> Eff (ref :: REF | eff) Number
foreign import oneMinuteRate :: forall eff. Meter -> Eff (ref :: REF | eff) Number
foreign import meanRate :: forall eff. Meter -> Eff (ref :: REF | eff) Number
foreign import count :: forall eff. Meter -> Eff (ref :: REF | eff) Int

new :: forall eff. Eff (ref :: REF | eff) Meter
new = _new

markN :: forall eff. Meter -> Int -> Eff (ref :: REF | eff) Unit
markN = runFn2 _markN

mark :: forall eff. Meter -> Eff (ref :: REF | eff) Unit
mark m = markN m 1

newtype Summary = Summary {
                    count :: Int
                  , m1 :: Number
                  , m5 :: Number
                  , m15 :: Number
                  , mean :: Number
                  }

derive instance eqSummary :: Eq Summary
derive instance genericSummary :: Generic Summary _
instance showSummary :: Show Summary where
  show = genericShow

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
