 module System.Metrics
  ( Store
  , MetricSampler(..)
  , Value(..)
  , newStore
  , register
  , registerOrGet
  , get
  , registerCounter
  , registerGauge
  , registerHistogram
  , registerMeter
  , registerTimer
  , createOrGetCounter
  , createOrGetGuage
  , createOrGetHistogramWithExponentialDecaySampling
  , createOrGetHistogramWithUniformSampling
  , createOrGetMeter
  , createOrGetTimer
  , sampleOne
  , sample
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef', newRef, readRef)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import System.Metrics.Counter (Counter)
import System.Metrics.Counter as Counter
import System.Metrics.Gauge (Gauge)
import System.Metrics.Gauge as Gauge
import System.Metrics.Histogram (Histogram)
import System.Metrics.Histogram as Histogram
import System.Metrics.Meter (Meter)
import System.Metrics.Meter as Meter
import System.Metrics.Timer (Timer)
import System.Metrics.Timer as Timer

-- | A mutable metric store.
newtype Store = Store (Ref State)

-- | The 'Store' state.
type State = Map.Map String MetricSampler

data MetricSampler = CounterS Counter
                   | GaugeS Gauge
                   | HistogramS Histogram
                   | MeterS Meter
                   | TimerS Timer

-- | Create a new, empty metric store.
newStore ::forall eff. Eff (ref :: REF | eff) Store
newStore = do
  ref <- newRef Map.empty
  pure $ Store ref

register :: forall eff.
            String -> MetricSampler -> Store -> Eff (ref :: REF | eff) Boolean
register name sampler (Store store) = modifyRef' store $ \state ->
  case Map.lookup name state of
    Just s -> { state : state, value : false }
    Nothing -> { state : Map.insert name sampler state, value : true }

registerOrGet :: forall eff.
            String -> MetricSampler -> Store -> Eff (ref :: REF | eff) MetricSampler
registerOrGet name sampler (Store store) = modifyRef' store $ \state ->
  case Map.lookup name state of
    Just s -> { state : state, value : s }
    Nothing -> { state : Map.insert name sampler state, value : sampler }

get :: forall eff. String -> Store -> Eff (ref :: REF | eff) (Maybe MetricSampler)
get name (Store store) = readRef store >>= pure <<< Map.lookup name

-- | Register a non-negative, monotonically increasing, integer-valued metric.
-- |
-- | Also see 'createCounter'.
registerCounter :: forall eff.
                   String -> Counter -> Store -> Eff (ref :: REF | eff) Boolean
registerCounter name counter = register name (CounterS counter)

-- | Create and register a zero-initialized counter. Throw exception is the counter is already created.
createOrGetCounter :: forall eff.
                 String
              -> Store
              -> Eff (ref :: REF, exception :: EXCEPTION | eff) Counter
createOrGetCounter name store = do
  counter <- Counter.new
  registerOrGet name (CounterS counter) store >>= case _ of
    CounterS c -> pure c
    _ -> throw $ "Metric name is already registered: " <> name

-- | Register an integer-valued metric.
-- |
-- | Also see 'createGuage'.
registerGauge :: forall eff.
                 String -> Gauge -> Store -> Eff (ref :: REF | eff) Boolean
registerGauge name gauge = register name (GaugeS gauge)

-- | Create and register a guage. Throw exception is the guage is already created.
createOrGetGuage :: forall eff.
               String
            -> (forall e. Aff e Int)
            -> Store
            -> Eff (ref :: REF, exception :: EXCEPTION | eff) Gauge
createOrGetGuage name f store = do
  let gauge = Gauge.new f
  registerOrGet name (GaugeS gauge) store >>= case _ of
    GaugeS g -> pure g
    _ -> throw $ "Metric name is already registered: " <> name

registerHistogram :: forall eff.
                     String -> Histogram -> Store -> Eff (ref :: REF | eff) Boolean
registerHistogram name hist = register name (HistogramS hist)

createOrGetHistogramWithExponentialDecaySampling
  :: forall eff.
     String
  -> Int
  -> Number
  -> Store
  -> Eff (ref :: REF, exception :: EXCEPTION | eff) Histogram
createOrGetHistogramWithExponentialDecaySampling name size alpha store = do
  hist <- Histogram.newWithExponentialDecaySampling size alpha
  registerOrGet name (HistogramS hist) store >>= case _ of
    HistogramS c -> pure c
    _ -> throw $ "Metric name is already registered: " <> name

createOrGetHistogramWithUniformSampling
  :: forall eff.
     String
  -> Int
  -> Store
  -> Eff (ref :: REF, exception :: EXCEPTION | eff) Histogram
createOrGetHistogramWithUniformSampling name size store = do
  hist <- Histogram.newWithUniformSampling size
  registerOrGet name (HistogramS hist) store >>= case _ of
    HistogramS c -> pure c
    _ -> throw $ "Metric name is already registered: " <> name

registerMeter :: forall eff. String -> Meter -> Store -> Eff (ref :: REF | eff) Boolean
registerMeter name meter = register name (MeterS meter)

createOrGetMeter
  :: forall eff.
     String
  -> Store
  -> Eff (ref :: REF, exception :: EXCEPTION | eff) Meter
createOrGetMeter name store = do
  meter <- Meter.new
  registerOrGet name (MeterS meter) store >>= case _ of
    MeterS c -> pure c
    _ -> throw $ "Metric name is already registered: " <> name

registerTimer :: forall eff. String -> Timer -> Store -> Eff (ref :: REF | eff) Boolean
registerTimer name timer = register name (TimerS timer)

createOrGetTimer
  :: forall eff.
     String
  -> Store
  -> Eff (ref :: REF, exception :: EXCEPTION | eff) Timer
createOrGetTimer name store = do
  timer <- Timer.new
  registerOrGet name (TimerS timer) store >>= case _ of
    TimerS c -> pure c
    _ -> throw $ "Metric name is already registered: " <> name

type Sample = Map.Map String Value

data Value = CounterV Int
           | GaugeV Int
           | HistogramV Histogram.Summary
           | MeterV Meter.Summary
           | TimerV Timer.Summary

derive instance eqVal :: Eq Value
derive instance genVal :: Generic Value _
instance showVal :: Show Value where
  show = genericShow

sampleOne :: forall eff. MetricSampler -> Aff (ref :: REF | eff) Value
sampleOne (CounterS c) = CounterV <$> liftEff (Counter.read c)
sampleOne (GaugeS g) = GaugeV <$> Gauge.read g
sampleOne (HistogramS h) = HistogramV <$> liftEff (Histogram.read h)
sampleOne (MeterS h) = MeterV <$> liftEff (Meter.read h)
sampleOne (TimerS h) = TimerV <$> liftEff (Timer.read h)

sample :: forall eff. Store -> Aff (ref :: REF | eff) Sample
sample (Store store) = do
  state <- liftEff $ readRef store
  sequence $ map sampleOne state

-- main = do
--   store <- newStore
--   counter <- createOrGetCounter "testc" store
--   gauge <- createOrGetGuage "testg" (pure 3) store
--   hist <- createOrGetHistogramWithExponentialDecaySampling "hizz" 1028 0.015 store
--   meter <- createOrGetMeter "mmm" store
--   timer <- createOrGetTimer "ttt" store
--   Counter.inc counter 2
--   Histogram.update hist 1.2
--   Histogram.update hist 2.1
--   Meter.mark meter
--   Timer.update timer (Milliseconds 1000.0)
--   launchAff $ sample store >>= logShow
