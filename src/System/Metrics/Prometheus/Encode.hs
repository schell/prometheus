{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module System.Metrics.Prometheus.Encode
       ( encodeMetrics
       , serializeMetrics
       ) where

import           Data.ByteString.Builder             (Builder, byteString,
                                                      char8, doubleDec, intDec,
                                                      toLazyByteString)
import           Data.ByteString.Lazy                (ByteString)
import           Data.List                           (intersperse)
import qualified Data.Map                            as Map
import           Data.Monoid                         ((<>))
import           Data.Text                           (Text)
import           Data.Text.Encoding                  (encodeUtf8)
import           Data.Text.Lazy                      (toStrict)
import           Data.Text.Lazy.Builder              (toLazyText)
import           Data.Text.Lazy.Builder.RealFloat    (FPFormat (Generic),
                                                      formatRealFloat)

import           System.Metrics.Prometheus.Counter   (CounterSample (..))
import           System.Metrics.Prometheus.Gauge     (GaugeSample (..))
import           System.Metrics.Prometheus.Histogram (Count,
                                                      HistogramSample (..),
                                                      UpperBound)
import           System.Metrics.Prometheus.MetricId  (Labels (..),
                                                      MetricId (..), Name (..),
                                                      addLabel)
import           System.Metrics.Prometheus.Sample    (MetricSample (..),
                                                      RegistrySample (..),
                                                      metricSample)


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

encodeHeader :: MetricId -> MetricSample -> Builder
encodeHeader mid sample
    =  "# HELP " <> nm <> space <> "help" <> newline
    <> "# TYPE " <> nm <> space <> encodeSampleType sample
  where nm = encodeName (name mid)


encodeCounter :: MetricId -> CounterSample -> Builder
encodeCounter mid counter = encodeMetricId mid <> space <> intDec (unCounterSample counter)


encodeGauge :: MetricId -> GaugeSample -> Builder
encodeGauge mid gauge = encodeMetricId mid <> space <> doubleDec (unGaugeSample gauge)


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


encodeSampleType :: MetricSample -> Builder
encodeSampleType = byteString . metricSample (const "counter")
    (const "gauge") (const "histogram") (const "summary")


encodeMetricId :: MetricId -> Builder
encodeMetricId mid = encodeName (name mid) <> encodeLabels (labels mid)


encodeName :: Name -> Builder
encodeName = text . unName


encodeLabels :: Labels -> Builder
encodeLabels ls
    | Map.null ls' = space
    | otherwise =
             openBracket
          <> (mconcat . intersperse comma . map encodeLabel $ Map.toList ls')
          <> closeBracket
  where ls' = unLabels ls


encodeLabel :: (Text, Text) -> Builder
encodeLabel (key, value) = text key <> equals <> text value


text :: Text -> Builder
text = byteString . encodeUtf8


space :: Builder
space = char8 ' '


newline :: Builder
newline = char8 '\n'


openBracket :: Builder
openBracket = char8 '{'


closeBracket :: Builder
closeBracket = char8 '}'


comma :: Builder
comma = char8 ','


equals :: Builder
equals = char8 '='
