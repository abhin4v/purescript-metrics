module System.Metrics.Gauge (Gauge, new, read) where

import Control.Monad.Aff (Aff)

-- | An integer valued metric.
newtype Gauge = Gauge (forall eff. Aff eff Int)

-- | Creates a new gauge given the underlying measurement function.
new :: (forall eff. Aff eff Int) -> Gauge
new = Gauge

-- | Returns the current value of the gauge.
read :: forall eff. Gauge -> Aff eff Int
read (Gauge f) = f
