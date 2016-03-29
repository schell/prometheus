{-# LANGUAGE OverloadedStrings #-}

module System.Metrics.Prometheus.Concurrent.Http where

import           Control.Monad.IO.Class                         (liftIO)
import           Control.Monad.Reader.Class                     (ask)
import           Data.Text                                      (Text)
import           Network.HTTP.Types                             (hContentType,
                                                                 methodGet,
                                                                 status200,
                                                                 status404)
import           Network.Wai                                    (Application,
                                                                 Response,
                                                                 pathInfo,
                                                                 requestMethod,
                                                                 responseBuilder,
                                                                 responseLBS)
import           Network.Wai.Handler.Warp                       (Port, run)

import           System.Metrics.Prometheus.Concurrent.Registry  (Registry,
                                                                 sample)
import           System.Metrics.Prometheus.Concurrent.RegistryT (RegistryT)
import           System.Metrics.Prometheus.Encode               (encodeMetrics)


type Path = [Text]


serveHttpTextMetricsDef :: Port -> Registry -> IO ()
serveHttpTextMetricsDef = flip serveHttpTextMetrics ["metrics"]


serveHttpTextMetrics :: Port -> Path -> Registry -> IO ()
serveHttpTextMetrics port path = run port . prometheusApp path


serveHttpTextMetricsT :: Port -> Path -> RegistryT IO ()
serveHttpTextMetricsT port path = liftIO . serveHttpTextMetrics port path =<< ask


prometheusApp :: Path -> Registry -> Application
prometheusApp path registry request respond
    | prometheusRequest = prometheusResponse respond registry
    | otherwise = respond $ responseLBS status404 header404 body404
  where
    prometheusRequest = requestMethod request == methodGet && pathInfo request == path
    header404 = [(hContentType, "text/plain")]
    body404 = "404"


prometheusResponse :: (Response -> IO b) -> Registry -> IO b
prometheusResponse respond gr =
    respond . responseBuilder status200 headers . encodeMetrics =<< sample gr
  where
    headers = [(hContentType, "text/plain; version=0.0.4")]
