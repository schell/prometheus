{-# LANGUAGE OverloadedStrings #-}

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
import           Data.Text                           (Text, replace)
import           Data.Text.Encoding                  (encodeUtf8)

import           System.Metrics.Prometheus.Counter   (CounterSample (..))
import           System.Metrics.Prometheus.Gauge     (GaugeSample (..))
import           System.Metrics.Prometheus.Histogram (HistogramSample (..))
import           System.Metrics.Prometheus.MetricId  (MetricId (..))
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
    <> metricSample (encodeCounter mid) (encodeGauge mid) encodeHistogram encodeSummary sample


encodeHeader :: MetricId -> MetricSample -> Builder
encodeHeader mid sample
    =  "# HELP " <> encodeName mid <> space <> "help" <> newline
    <> "# TYPE " <> encodeName mid <> space <> encodeSampleType sample


encodeHistogram = undefined
encodeSummary = undefined


encodeCounter :: MetricId -> CounterSample -> Builder
encodeCounter mid counter = encodeMetricId mid <> space <> intDec (unCounterSample counter)


encodeGauge :: MetricId -> GaugeSample -> Builder
encodeGauge mid gauge = encodeMetricId mid <> space <> doubleDec (unGaugeSample gauge)


encodeSampleType :: MetricSample -> Builder
encodeSampleType = byteString . metricSample (const "counter")
    (const "gauge") (const "histogram") (const "summary")


encodeMetricId :: MetricId -> Builder
encodeMetricId mid = encodeName mid <> encodeLabels mid


encodeName :: MetricId -> Builder
encodeName = text . name


encodeLabels :: MetricId -> Builder
encodeLabels mid | Map.null (labels mid) = space
                 | otherwise =
                   openBracket
                   <> (mconcat . intersperse comma . map encodeLabel . Map.toList $ labels mid)
                   <> closeBracket


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
