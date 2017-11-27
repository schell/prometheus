{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module System.Metrics.Prometheus.Registry
       ( Registry
       , RegistrySample (..)
       , new
       , registerCounter
       , registerGauge
       , registerHistogram
       , sample
       ) where

import           Control.Exception                          (Exception, throw)
import           Control.Lens.Operators
import           Data.Map                                   (Map)
import qualified Data.Map                                   as Map
import           Data.ByteString.Lazy.Builder               (Builder,
                                                             toLazyByteString)
import           Data.ProtoLens                             (def)
import           Data.ProtoLens.Encoding                    (buildMessageDelimited)
import           Data.Typeable                              (Typeable)
import           Network.HTTP.Client                        (requestBody,
                                                             RequestBody(..),
                                                             requestHeaders)
import           Network.Wreq.Types                         (Putable (..))

import qualified Proto.Proto.Metrics                        as Proto
import qualified Proto.Proto.Metrics'Fields                 as Proto
import           System.Metrics.Prometheus.Metric           (Metric (..),
                                                             MetricSample (..))
import           System.Metrics.Prometheus.Metric.Counter   (Counter)
import qualified System.Metrics.Prometheus.Metric.Counter   as Counter
import           System.Metrics.Prometheus.Metric.Gauge     (Gauge)
import qualified System.Metrics.Prometheus.Metric.Gauge     as Gauge
import           System.Metrics.Prometheus.Metric.Histogram (Histogram,
                                                             UpperBound)
import qualified System.Metrics.Prometheus.Metric.Histogram as Histogram
import qualified System.Metrics.Prometheus.Metric.Summary   as Summary
import           System.Metrics.Prometheus.MetricId         (Labels(..),
                                                             MetricId (MetricId),
                                                             Name(..))

newtype Registry = Registry { unRegistry :: Map MetricId Metric }
newtype RegistrySample = RegistrySample { unRegistrySample :: Map MetricId MetricSample }

newtype KeyError = KeyError MetricId deriving (Show, Typeable)
instance Exception KeyError


new :: Registry
new = Registry Map.empty


registerCounter :: Name -> Labels -> Registry -> IO (Counter, Registry)
registerCounter name labels registry = do
    counter <- Counter.new
    return (counter, Registry $ Map.insertWithKey collision mid (CounterMetric counter) (unRegistry registry))
  where
      mid = MetricId name labels
      collision k _ _ = throw (KeyError k)


registerGauge :: Name -> Labels -> Registry -> IO (Gauge, Registry)
registerGauge name labels registry = do
    gauge <- Gauge.new
    return (gauge, Registry $ Map.insertWithKey collision mid (GaugeMetric gauge) (unRegistry registry))
  where
      mid = MetricId name labels
      collision k _ _ = throw (KeyError k)


registerHistogram :: Name -> Labels -> [UpperBound] -> Registry -> IO (Histogram, Registry)
registerHistogram name labels buckets registry = do
    histogram <- Histogram.new buckets
    return (histogram, Registry $ Map.insertWithKey collision mid (HistogramMetric histogram) (unRegistry registry))
  where
      mid = MetricId name labels
      collision k _ _ = throw (KeyError k)


sample :: Registry -> IO RegistrySample
sample = fmap RegistrySample . mapM sampleMetric . unRegistry
  where
    sampleMetric :: Metric -> IO MetricSample
    sampleMetric (CounterMetric count) = CounterMetricSample <$> Counter.sample count
    sampleMetric (GaugeMetric gauge) = GaugeMetricSample <$> Gauge.sample gauge
    sampleMetric (HistogramMetric histogram) = HistogramMetricSample <$> Histogram.sample histogram

serializeRegistrySample :: RegistrySample -> Builder
serializeRegistrySample (RegistrySample rs) =
  Map.foldMapWithKey ((buildMessageDelimited  .) . serializeMetricSample) rs

serializeMetricSample :: MetricId -> MetricSample -> Proto.MetricFamily
serializeMetricSample (MetricId (Name name) (Labels labels)) = go
  where
    base :: Proto.MetricFamily
    base = def & Proto.name .~ name
    baseMetric = def & Proto.label .~ labels'
    labels' =
      Map.foldrWithKey
        (\n v -> ((def & Proto.name .~ n & Proto.value .~ v) :))
        []
        labels
    go (CounterMetricSample (Counter.CounterSample i)) =
      base & Proto.type' .~ Proto.COUNTER
           & Proto.metric .~
             [ baseMetric & Proto.counter . Proto.value .~
                 fromIntegral i
             ]
    go (GaugeMetricSample (Gauge.GaugeSample i)) =
      base & Proto.type' .~ Proto.GAUGE
           & Proto.metric .~
             [ baseMetric & Proto.gauge . Proto.value .~ i
             ]
    go (HistogramMetricSample (Histogram.HistogramSample buckets s count)) =
      base & Proto.type' .~ Proto.HISTOGRAM
           & Proto.metric .~
             [ baseMetric & Proto.histogram .~
                 (def & Proto.sampleCount .~ fromIntegral count
                      & Proto.sampleSum .~ s
                      & Proto.bucket .~
                          Map.foldrWithKey
                            (\ub c -> ((def & Proto.cumulativeCount .~
                                                round c
                                            & Proto.upperBound .~ ub
                                       ) :))
                            []
                            buckets
                 )
             ]
    go (SummaryMetricSample (Summary.SummarySample quantiles s count)) =
      base & Proto.type' .~ Proto.SUMMARY
           & Proto.metric .~
             [ baseMetric & Proto.summary .~
               (def & Proto.sampleCount .~ fromIntegral count
                    & Proto.sampleSum .~ fromIntegral s
                    & Proto.quantile .~
                        Map.foldrWithKey
                          (\q v -> ((def & Proto.quantile .~ q
                                         & Proto.value .~ fromIntegral v
                                    ) :))
                          []
                          quantiles
               )
             ]

instance Putable RegistrySample where
  putPayload s req = pure $ req
    { requestBody =
        RequestBodyLBS . toLazyByteString $ serializeRegistrySample s
    , requestHeaders =
        ( "Content-Type"
        , "application/vnd.google.protobuf; proto=io.prometheus.client.MetricFamily; encoding=delimited"
        ) : requestHeaders req
    }
