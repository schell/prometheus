module System.Metrics.Prometheus.Metric where

import qualified System.Metrics.Prometheus.Counter   as C
import qualified System.Metrics.Prometheus.Gauge     as G
import qualified System.Metrics.Prometheus.Histogram as H
import qualified System.Metrics.Prometheus.Summary   as S

data Metric
    = Counter C.Counter
    | Gauge G.Gauge
    -- | Summary S.Summary
    | Histogram H.Histogram
