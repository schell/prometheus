{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module System.Metrics.Prometheus.Encode
       ( encodeMetrics
       , serializeMetrics
       ) where

import           Data.ByteString.Builder                    (Builder, doubleDec,
                                                             intDec,
                                                             toLazyByteString)
import           Data.ByteString.Lazy                       (ByteString)
import           Data.List                                  (intersperse)
import qualified Data.Map                                   as Map
import           Data.Monoid                                ((<>))

import           System.Metrics.Prometheus.Encode.Histogram (encodeHistogram)
import           System.Metrics.Prometheus.Encode.MetricId  (encodeHeader,
                                                             encodeMetricId,
                                                             newline, space)
import           System.Metrics.Prometheus.Metric           (MetricSample (..),
                                                             metricSample)
import           System.Metrics.Prometheus.Metric.Counter   (CounterSample (..))
import           System.Metrics.Prometheus.Metric.Gauge     (GaugeSample (..))
import           System.Metrics.Prometheus.MetricId         (MetricId (..))
import           System.Metrics.Prometheus.Registry         (RegistrySample (..))


serializeMetrics :: RegistrySample -> ByteString
serializeMetrics = toLazyByteString . encodeMetrics


encodeMetrics :: RegistrySample -> Builder
encodeMetrics = mconcat . intersperse "\n\n" .
    map (uncurry encodeMetric) . Map.toList . unRegistrySample


encodeMetric :: MetricId -> MetricSample -> Builder
encodeMetric mid sample
    =  encodeHeader mid sample <> newline
    <> metricSample (encodeCounter mid) (encodeGauge mid) (encodeHistogram mid) (encodeSummary mid) sample
  where
    encodeSummary = undefined


encodeCounter :: MetricId -> CounterSample -> Builder
encodeCounter mid counter = encodeMetricId mid <> space <> intDec (unCounterSample counter)


encodeGauge :: MetricId -> GaugeSample -> Builder
encodeGauge mid gauge = encodeMetricId mid <> space <> doubleDec (unGaugeSample gauge)
