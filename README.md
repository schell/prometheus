# Prometheus Haskell Client

A simple and modern, type safe, idiomatic Haskell client for
[Prometheus](http://prometheus.io) monitoring. Specifically there is
no use of unsafe IO or manual ByteString construction from lists of
bytes. Batteries-included web server.  .  [Usage Example]

- [Hackage Package](https://hackage.haskell.org/package/prometheus)
- [Github](http://github.com/LukeHoersten/prometheus)

## Usage Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Example where

import           System.Metrics.Prometheus.GlobalRegistry
import           System.Metrics.Prometheus.Http
import           System.Metrics.Prometheus.Metric.Counter (inc)
import           System.Metrics.Prometheus.MetricId


main :: IO ()
main = do
    globalRegistry <- new

    -- Labels can be defined as lists or added to an empty label set
    connectSuccessGauge <- registerGauge "example_connections" (fromList [("login", "success")]) globalRegistry
    connectFailureGauge <- registerGauge "example_connections" (addLabel "login" "failure" mempty) globalRegistry
    connectCounter <- registerCounter "example_connection_total" mempty globalRegistry
    latencyHistogram <- registerHistogram "example_round_trip_latency_ms" mempty [10, 20..100] globalRegistry

    inc connectCounter -- increment a counter

    -- [...] pass metric handles to the rest of the app

    serveHttpTextMetrics 8080 globalRegistry -- http://localhost:8080/metric server
```

## Advanced Usage

A `Registry` and `StateT`-based `RegistryT` are available for unit
testing or generating lists of `[IO a]` actions that can be
`sequenced` and returned from pure code to be applied.

## Tasks

- [ ] Implement help docstrings.
- [ ] Implement GHC-specific metrics.
- [ ] Implement [summary metric](https://github.com/prometheus/client_golang/blob/master/prometheus/summary.go).
- [ ] Encode name and labels on register.
- [ ] Implement ReaderT for GlobalRegistry.
- [ ] Library documentation and example.
- [ ] [Name and label validation](http://prometheus.io/docs/concepts/data_model/#metric-names-and-labels)
