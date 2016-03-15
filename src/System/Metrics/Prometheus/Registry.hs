module System.Metrics.Prometheus.Registry where

import           Data.Map                           (Map)
import qualified Data.Map                           as Map

import           System.Metrics.Prometheus.Counter  (Counter)
import qualified System.Metrics.Prometheus.Counter  as Counter
import           System.Metrics.Prometheus.Gauge    (Gauge)
import qualified System.Metrics.Prometheus.Gauge    as Gauge
import           System.Metrics.Prometheus.Metric   (Metric)
import qualified System.Metrics.Prometheus.Metric   as Metric
import           System.Metrics.Prometheus.MetricId (MetricId)


newtype Registry = Registry { unRegistry :: Map MetricId Metric }


registerCounter :: MetricId -> Registry -> IO (Counter, Registry)
registerCounter mid registry = do
    counter <- Counter.new
    return (counter, Registry $ Map.insertWithKey collision mid (Metric.Counter counter) (unRegistry registry))
  where collision k n o = error "oh shit"


registerGauge :: MetricId -> Registry -> IO (Gauge, Registry)
registerGauge mid registry = do
    gauge <- Gauge.new
    return (gauge, Registry $ Map.insertWithKey collision mid (Metric.Gauge gauge) (unRegistry registry))
  where collision k n o = error "oh shit"
