module System.Metrics.Prometheus.MetricId where

import           Data.Map  (Map)
import           Data.Text (Text)


data MetricId =
    MetricId
    { name   :: Text
    , labels :: Map Text Text
    } deriving (Eq, Ord, Show)
