{-# LANGUAGE OverloadedStrings #-}

module System.Metrics.Prometheus.Http where


import           Network.HTTP.Types                       (hContentType,
                                                           methodGet, status200,
                                                           status404)
import           Network.Wai                              (Application,
                                                           Response, pathInfo,
                                                           requestMethod,
                                                           responseBuilder,
                                                           responseLBS)
import           Network.Wai.Handler.Warp                 (Port, run)

import           System.Metrics.Prometheus.Encode         (encodeMetrics)
import           System.Metrics.Prometheus.GlobalRegistry (GlobalRegistry,
                                                           sample)


serveHttpTextMetrics :: Port -> GlobalRegistry -> IO ()
serveHttpTextMetrics port = run port . prometheusApp


prometheusApp :: GlobalRegistry -> Application
prometheusApp globalRegistry request respond
    | prometheusRequest = prometheusResponse respond globalRegistry
    | otherwise = respond $ responseLBS status404 header404 body404
  where
    prometheusRequest = requestMethod request == methodGet && pathInfo request == ["metrics"]
    header404 = [(hContentType, "text/plain")]
    body404 = "404"


prometheusResponse :: (Response -> IO b) -> GlobalRegistry -> IO b
prometheusResponse respond gr =
    respond . responseBuilder status200 headers . encodeMetrics =<< sample gr
  where
      headers = [(hContentType, "text/plain; version=0.0.4")]
