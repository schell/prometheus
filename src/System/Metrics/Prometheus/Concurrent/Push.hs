module System.Metrics.Prometheus.Concurrent.Push
       ( pushHttpProtoMetrics
       )
       where

import Control.Concurrent                 (threadDelay)
import Control.Monad                      (forever)
import Data.Map                           (foldMapWithKey)
import Data.Text                          (Text, unpack)
import Network.Wreq.Session               (withSession, put)
import System.Metrics.Prometheus.MetricId (Labels(..))
import System.Metrics.Prometheus.Registry (RegistrySample)

-- | Push metrics to a pushgateway.
pushHttpProtoMetrics :: String -- ^ The base URL of the pushgateway,
                               -- including the port number.
                     -> Text   -- ^ The name of this job.
                     -> Labels -- ^ The label set to use as a grouping
                               -- key for these metrics.
                     -> Int -- ^ Push frequency, in microseconds.
                     -> IO RegistrySample -- ^ The action to get the
                                          -- latest metrics.
                     -> IO ()
pushHttpProtoMetrics base job (Labels ls) frequency get =
    withSession $ \session -> forever $
      get >>= put session url >> threadDelay frequency
  where
    url = base ++ "/metrics/job/" ++ (unpack job) ++
      foldMapWithKey (\k v -> "/" ++ unpack k ++ "/" ++ unpack v) ls
