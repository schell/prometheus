module System.Metrics.Prometheus.Sample where


import           Data.Map                            (Map)

import           System.Metrics.Prometheus.Counter   (CounterSample (..))
import qualified System.Metrics.Prometheus.Counter   as Counter
import           System.Metrics.Prometheus.Gauge     (GaugeSample (..))
import qualified System.Metrics.Prometheus.Gauge     as Gauge
import           System.Metrics.Prometheus.Histogram (HistogramSample (..))
import qualified System.Metrics.Prometheus.Histogram as Histogram
import           System.Metrics.Prometheus.Metric    (Metric)
import qualified System.Metrics.Prometheus.Metric    as Metric
import           System.Metrics.Prometheus.MetricId  (MetricId)
import           System.Metrics.Prometheus.Registry  (Registry, unRegistry)


data SummarySample =
    SummarySample
    { sumQuantiles :: Map Double Int
    , sumSum       :: Int
    , sumCount     :: Int
    }


data MetricSample
    = Counter CounterSample
    | Gauge GaugeSample
    | Histogram HistogramSample
    | Summary SummarySample


metricSample :: (CounterSample -> a) -> (GaugeSample -> a)
             -> (HistogramSample -> a) -> (SummarySample -> a)
             -> MetricSample -> a
metricSample f _ _ _ (Counter s)   = f s
metricSample _ f _ _ (Gauge s)     = f s
metricSample _ _ f _ (Histogram s) = f s
metricSample _ _ _ f (Summary s)   = f s


newtype RegistrySample = RegistrySample { unRegistrySample :: Map MetricId MetricSample }


sample :: Registry -> IO RegistrySample
sample = fmap RegistrySample . mapM sampleMetric . unRegistry


sampleMetric :: Metric -> IO MetricSample
sampleMetric (Metric.Counter count) = Counter <$> Counter.sample count
sampleMetric (Metric.Gauge gauge) = Gauge <$> Gauge.sample gauge
sampleMetric (Metric.Histogram histogram) = Histogram <$> Histogram.sample histogram
