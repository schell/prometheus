{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module System.Metrics.Prometheus.Encode.Histogram where


import           Data.ByteString.Builder                    (Builder, doubleDec,
                                                             intDec)
import qualified Data.Map                                   as Map
import           Data.Monoid                                ((<>))
import           Data.Text.Lazy                             (toStrict)
import           Data.Text.Lazy.Builder                     (toLazyText)
import           Data.Text.Lazy.Builder.RealFloat           (FPFormat (Generic),
                                                             formatRealFloat)

import           System.Metrics.Prometheus.Encode.MetricId  (encodeLabels,
                                                             encodeName, space)
import           System.Metrics.Prometheus.Metric.Histogram (Count, HistogramSample (..),
                                                             UpperBound)
import           System.Metrics.Prometheus.MetricId         (MetricId (..),
                                                             addLabel)


encodeHistogram :: MetricId -> HistogramSample -> Builder
encodeHistogram mid histogram
    =  encodeHistogramBuckets mid histogram
    <> encodeName (name mid) <> "_sum" <> space <> doubleDec (histSum histogram)
    <> encodeName (name mid) <> "_count" <> space <> intDec (histCount histogram)


encodeHistogramBuckets :: MetricId -> HistogramSample -> Builder
encodeHistogramBuckets mid = mconcat . map snd . Map.toList
    . Map.mapWithKey (encodeHistogramBucket mid) . histBuckets


encodeHistogramBucket :: MetricId -> UpperBound -> Count -> Builder
encodeHistogramBucket mid upperBound count
    =  encodeName (name mid) <> "_bucket" <> encodeLabels labels' <> space <> intDec count
  where
    upperBoundValue = toStrict . toLazyText $ formatRealFloat Generic Nothing upperBound
    labels' = addLabel "le" upperBoundValue $ labels mid
