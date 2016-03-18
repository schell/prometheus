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
