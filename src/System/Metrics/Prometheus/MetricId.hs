module System.Metrics.Prometheus.MetricId where

import           Data.Map  (Map)
import qualified Data.Map  as Map
import           Data.Text (Text)


newtype Name = Name { unName :: Text } deriving (Show, Eq, Ord)
newtype Labels = Labels { unLabels :: Map Text Text } deriving (Show, Eq, Ord)


data MetricId =
    MetricId
    { name   :: Name
    , labels :: Labels
    } deriving (Eq, Ord, Show)


addLabel :: Text -> Text -> Labels -> Labels
addLabel key val = Labels . Map.insert key val . unLabels
