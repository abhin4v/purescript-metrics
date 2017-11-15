module System.Metrics.Counter (Counter, new, read, inc) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Data.Function.Uncurried (Fn2, runFn2)
import Prelude (Unit)

foreign import data Counter :: Type
foreign import _new :: forall eff. Eff (ref :: REF | eff) Counter
foreign import read :: forall eff. Counter -> Eff (ref :: REF | eff) Int
foreign import reset :: forall eff. Counter -> Eff (ref :: REF | eff) Unit
foreign import _inc :: forall eff. Fn2 Counter Int (Eff (ref :: REF | eff) Unit)

new :: forall eff. Eff (ref :: REF | eff) Counter
new =  _new

inc :: forall eff. Counter -> Int -> Eff (ref :: REF | eff) Unit
inc c = runFn2 _inc c
