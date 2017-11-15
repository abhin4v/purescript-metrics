module System.Metrics.Gauge (Gauge, new, read) where

import Control.Monad.Aff (Aff)

newtype Gauge = Gauge (forall eff. Aff eff Int)

new :: (forall eff. Aff eff Int) -> Gauge
new = Gauge

read :: forall eff. Gauge -> Aff eff Int
read (Gauge f) = f
