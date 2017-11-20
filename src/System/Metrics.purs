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
  , createOrGetGauge
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
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, genericEncode)
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

-- | Different types of metric samplers.
data MetricSampler = CounterS Counter
                   | GaugeS Gauge
                   | HistogramS Histogram
                   | MeterS Meter
                   | TimerS Timer

-- | Creates a new, empty metric store.
newStore ::forall eff. Eff (ref :: REF | eff) Store
newStore = do
  ref <- newRef Map.empty
  pure $ Store ref

-- | Registers a metric sampler with the given name, to the given metric store.
-- | Returns true if registration succeeds, false if registration fails because another metric sampler
-- | is already registered with the given name.
register :: forall eff.
            String -> MetricSampler -> Store -> Eff (ref :: REF | eff) Boolean
register name sampler (Store store) = modifyRef' store $ \state ->
  case Map.lookup name state of
    Just s -> { state : state, value : false }
    Nothing -> { state : Map.insert name sampler state, value : true }

-- | Registers and returns a metric sampler with the given name, to the given metric store if another one
-- | is not registered with the given name. Else, returns the metric sampler registered with the given name.
registerOrGet :: forall eff.
            String -> MetricSampler -> Store -> Eff (ref :: REF | eff) MetricSampler
registerOrGet name sampler (Store store) = modifyRef' store $ \state ->
  case Map.lookup name state of
    Just s -> { state : state, value : s }
    Nothing -> { state : Map.insert name sampler state, value : sampler }

-- | Returns a metric sampler registered to the given metric store with the given name.
get :: forall eff. String -> Store -> Eff (ref :: REF | eff) (Maybe MetricSampler)
get name (Store store) = readRef store >>= pure <<< Map.lookup name

-- | Register a Counter with the given name to the given store.
-- | Returns true if registration succeeds, false if registration fails because another metric sampler
-- | is already registered with the given name.
registerCounter :: forall eff.
                   String -> Counter -> Store -> Eff (ref :: REF | eff) Boolean
registerCounter name counter = register name (CounterS counter)

-- | Creates, registers and returns a Counter with the given name to the given store.
-- | If a Counter is already registered with the given name, returns it.
-- | Throws exception if a non-Counter metric sampler is already registered with the given name.
createOrGetCounter :: forall eff.
                 String
              -> Store
              -> Eff (ref :: REF, exception :: EXCEPTION | eff) Counter
createOrGetCounter name store = do
  counter <- Counter.new
  registerOrGet name (CounterS counter) store >>= case _ of
    CounterS c -> pure c
    _ -> throw $ "Metric name is already registered: " <> name

-- | Register a Gauge with the given name to the given store.
-- | Returns true if registration succeeds, false if registration fails because another metric sampler
-- | is already registered with the given name.
registerGauge :: forall eff.
                 String -> Gauge -> Store -> Eff (ref :: REF | eff) Boolean
registerGauge name gauge = register name (GaugeS gauge)

-- | Creates, registers and returns a Gauge with the given name to the given store.
-- | If a Gauge is already registered with the given name, returns it.
-- | Throws exception if a non-Gauge metric sampler is already registered with the given name.
createOrGetGauge :: forall eff.
               String
            -> (forall e. Aff e Int)
            -> Store
            -> Eff (ref :: REF, exception :: EXCEPTION | eff) Gauge
createOrGetGauge name f store = do
  let gauge = Gauge.new f
  registerOrGet name (GaugeS gauge) store >>= case _ of
    GaugeS g -> pure g
    _ -> throw $ "Metric name is already registered: " <> name

-- | Register a Histogram with the given name to the given store.
-- | Returns true if registration succeeds, false if registration fails because another metric sampler
-- | is already registered with the given name.
registerHistogram :: forall eff.
                     String -> Histogram -> Store -> Eff (ref :: REF | eff) Boolean
registerHistogram name hist = register name (HistogramS hist)

-- | Creates, registers and returns a Histogram with exponential decay sampling, with the given name to the given store.
-- | If a Histogram is already registered with the given name, returns it.
-- | Throws exception if a non-Histogram metric sampler is already registered with the given name.
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

-- | Creates, registers and returns a Histogram with uniform sampling, with the given name to the given store.
-- | If a Histogram is already registered with the given name, returns it.
-- | Throws exception if a non-Histogram metric sampler is already registered with the given name.
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

-- | Register a Meter with the given name to the given store.
-- | Returns true if registration succeeds, false if registration fails because another metric sampler
-- | is already registered with the given name.
registerMeter :: forall eff. String -> Meter -> Store -> Eff (ref :: REF | eff) Boolean
registerMeter name meter = register name (MeterS meter)

-- | Creates, registers and returns a Meter with the given name to the given store.
-- | If a Meter is already registered with the given name, returns it.
-- | Throws exception if a non-Meter metric sampler is already registered with the given name.
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

-- | Register a Timer with the given name to the given store.
-- | Returns true if registration succeeds, false if registration fails because another metric sampler
-- | is already registered with the given name.
registerTimer :: forall eff. String -> Timer -> Store -> Eff (ref :: REF | eff) Boolean
registerTimer name timer = register name (TimerS timer)

-- | Creates, registers and returns a Timer with the given name to the given store.
-- | If a Timer is already registered with the given name, returns it.
-- | Throws exception if a non-Timer metric sampler is already registered with the given name.
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

-- | A sample of the metics in a metric store represented as a map with the metric names as the keys.
type Sample = Map.Map String Value

-- | Value of different metric samplers. Counter and Gauge values are their Int values.
-- | Histogram, Meter, and Timer values are the summary records of their values.
data Value = CounterV Int
           | GaugeV Int
           | HistogramV Histogram.Summary
           | MeterV Meter.Summary
           | TimerV Timer.Summary

derive instance eqVal :: Eq Value
derive instance genVal :: Generic Value _
instance showVal :: Show Value where
  show = genericShow
instance encodeVal :: Encode Value where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

-- | Samples the value of a metric sampler.
sampleOne :: forall eff. MetricSampler -> Aff (ref :: REF | eff) Value
sampleOne (CounterS c) = CounterV <$> liftEff (Counter.read c)
sampleOne (GaugeS g) = GaugeV <$> Gauge.read g
sampleOne (HistogramS h) = HistogramV <$> liftEff (Histogram.read h)
sampleOne (MeterS h) = MeterV <$> liftEff (Meter.read h)
sampleOne (TimerS h) = TimerV <$> liftEff (Timer.read h)

-- | Samples the value of all metric samplers in the given store.
sample :: forall eff. Store -> Aff (ref :: REF | eff) Sample
sample (Store store) = do
  state <- liftEff $ readRef store
  sequence $ map sampleOne state

-- main = do
--   store <- newStore
--   counter <- createOrGetCounter "testc" store
--   gauge <- createOrGetGauge "testg" (pure 3) store
--   hist <- createOrGetHistogramWithExponentialDecaySampling "hizz" 1028 0.015 store
--   meter <- createOrGetMeter "mmm" store
--   timer <- createOrGetTimer "ttt" store
--   Counter.inc counter 2
--   Histogram.update hist 1.2
--   Histogram.update hist 2.1
--   Meter.mark meter
--   Timer.update timer (Milliseconds 1000.0)
--   launchAff $ sample store >>= logShow
