{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module System.Metrics.Prometheus.Registry where

import           Control.Exception                   (Exception, throw)
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Typeable                       (Typeable)

import           System.Metrics.Prometheus.Counter   (Counter)
import qualified System.Metrics.Prometheus.Counter   as Counter
import           System.Metrics.Prometheus.Gauge     (Gauge)
import qualified System.Metrics.Prometheus.Gauge     as Gauge
import           System.Metrics.Prometheus.Histogram (Histogram)
import qualified System.Metrics.Prometheus.Histogram as Histogram
import           System.Metrics.Prometheus.Metric    (Metric)
import qualified System.Metrics.Prometheus.Metric    as Metric
import           System.Metrics.Prometheus.MetricId  (Labels,
                                                      MetricId (MetricId), Name)


newtype Registry = Registry { unRegistry :: Map MetricId Metric }


newtype KeyError = KeyError MetricId deriving (Show, Typeable)
instance Exception KeyError


registerCounter :: Name -> Labels -> Registry -> IO (Counter, Registry)
registerCounter name labels registry = do
    counter <- Counter.new
    return (counter, Registry $ Map.insertWithKey collision mid (Metric.Counter counter) (unRegistry registry))
  where
      mid = MetricId name labels
      collision k _ _ = throw (KeyError k)


registerGauge :: Name -> Labels -> Registry -> IO (Gauge, Registry)
registerGauge name labels registry = do
    gauge <- Gauge.new
    return (gauge, Registry $ Map.insertWithKey collision mid (Metric.Gauge gauge) (unRegistry registry))
  where
      mid = MetricId name labels
      collision k _ _ = throw (KeyError k)


registerHistogram :: Name -> Labels -> [Histogram.UpperBound] -> Registry -> IO (Histogram, Registry)
registerHistogram name labels buckets registry = do
    histogram <- Histogram.new buckets
    return (histogram, Registry $ Map.insertWithKey collision mid (Metric.Histogram histogram) (unRegistry registry))
  where
      mid = MetricId name labels
      collision k _ _ = throw (KeyError k)
