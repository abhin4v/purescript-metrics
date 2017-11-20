# purescript-metrics

A metrics library for PureScript inspired by the Haskell library [ekg](https://github.com/tibbe/ekg-core). It is a wrapper upon the JavaScript [metrics library](https://github.com/mikejihbe/metrics) which itself is a port of the Java Dropwizard [metrics library](http://metrics.dropwizard.io/).

API documentation can be found in [Pursuit](https://pursuit.purescript.org/packages/purescript-metrics).

## Sample Usage

```haskell
main = do
  store <- newStore
  counter <- createOrGetCounter "counter" store
  gauge <- createOrGetGauge "gauge" (pure 3) store
  hist <- createOrGetHistogramWithExponentialDecaySampling "hist" 1028 0.015 store
  meter <- createOrGetMeter "meter" store
  timer <- createOrGetTimer "timer" store
  Counter.inc counter 2
  Histogram.update hist 1.2
  Histogram.update hist 2.1
  Meter.mark meter
  Timer.update timer (Milliseconds 1000.0)
  launchAff $ sample store >>= logShow
```
