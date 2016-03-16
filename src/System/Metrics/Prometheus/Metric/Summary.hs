module System.Metrics.Prometheus.Metric.Summary where

import           Data.Map (Map)

data SummarySample =
    SummarySample
    { sumQuantiles :: Map Double Int
    , sumSum       :: Int
    , sumCount     :: Int
    }
