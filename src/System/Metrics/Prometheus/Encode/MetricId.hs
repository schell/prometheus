{-# LANGUAGE OverloadedStrings #-}

module System.Metrics.Prometheus.Encode.MetricId where

import           Data.ByteString.Builder            (Builder, byteString, char8)
import           Data.List                          (intersperse)
import qualified Data.Map                           as Map
import           Data.Monoid                        ((<>))
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (encodeUtf8)

import           System.Metrics.Prometheus.Metric   (MetricSample (..),
                                                     metricSample)
import           System.Metrics.Prometheus.MetricId (Labels (..), MetricId (..),
                                                     Name (..))


encodeHeader :: MetricId -> MetricSample -> Builder
encodeHeader mid sample
    = "# TYPE " <> nm <> space <> encodeSampleType sample
    -- <> "# HELP " <> nm <> space <> "help" <> newline <>
  where nm = encodeName (name mid)


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
encodeLabel (key, value) = text key <> equals <> quote <> text value <> quote


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


quote :: Builder
quote = char8 '"'
