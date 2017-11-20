module System.Metrics.Counter (Counter, new, read, reset, inc) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Data.Function.Uncurried (Fn2, runFn2)
import Prelude (Unit)

-- | A mutable, integer valued counter.
foreign import data Counter :: Type
foreign import _new :: forall eff. Eff (ref :: REF | eff) Counter

-- | Returns the current value of the counter.
foreign import read :: forall eff. Counter -> Eff (ref :: REF | eff) Int

-- | Resets the counter value to zero.
foreign import reset :: forall eff. Counter -> Eff (ref :: REF | eff) Unit
foreign import _inc :: forall eff. Fn2 Counter Int (Eff (ref :: REF | eff) Unit)

-- | Creates a new counter with zero value.
new :: forall eff. Eff (ref :: REF | eff) Counter
new =  _new

-- | Increments the counter value by the given count.
inc :: forall eff. Counter -> Int -> Eff (ref :: REF | eff) Unit
inc c = runFn2 _inc c
