{-# LANGUAGE OverloadedStrings #-}

module System.Metrics.Prometheus.Concurrent.Http where

import           Control.Monad.IO.Class                         (liftIO)
import           Data.Text                                      (Text)
import           Network.HTTP.Types                             (hContentType,
                                                                 methodGet,
                                                                 status200,
                                                                 status404)
import           Network.Wai                                    (Application,
                                                                 Request,
                                                                 Response,
                                                                 pathInfo,
                                                                 requestMethod,
                                                                 responseBuilder,
                                                                 responseLBS)
import           Network.Wai.Handler.Warp                       (Port, run)

import           System.Metrics.Prometheus.Concurrent.RegistryT (RegistryT,
                                                                 sample)
import           System.Metrics.Prometheus.Encode               (encodeMetrics)
import           System.Metrics.Prometheus.Registry             (RegistrySample)


type Path = [Text]


serveHttpTextMetrics :: Port -> Path -> IO RegistrySample -> IO ()
serveHttpTextMetrics port path = run port . app path


serveHttpTextMetricsT :: Port -> Path -> RegistryT IO ()
serveHttpTextMetricsT port path = liftIO . serveHttpTextMetrics port path =<< sample


app :: Path -> IO RegistrySample -> Application
app path runSample request respond
    | isPrometheusRequest path request = respond =<< prometheusResponse <$> runSample
    | otherwise = respond response404
  where
    prometheusResponse = responseBuilder status200 headers . encodeMetrics
    headers = [(hContentType, "text/plain; version=0.0.4")]


response404 :: Response
response404 = responseLBS status404 header404 body404
  where
    header404 = [(hContentType, "text/plain")]
    body404 = "404"


isPrometheusRequest :: Path -> Request -> Bool
isPrometheusRequest path request = isGet && matchesPath
  where
    matchesPath = pathInfo request == path
    isGet = requestMethod request == methodGet
