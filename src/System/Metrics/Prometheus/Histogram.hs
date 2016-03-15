{-# LANGUAGE TupleSections #-}

module System.Metrics.Prometheus.Histogram where


import           Data.Foldable (foldl')
import           Data.IORef    (IORef, atomicModifyIORef', newIORef, readIORef)
import           Data.Map      (Map)
import qualified Data.Map      as Map


newtype Histogram = Histogram { unHistogram :: IORef HistogramSample }


type UpperBound = Double -- Inclusive upper bounds
type Count = Int
type Buckets = Map UpperBound Count


data HistogramSample =
    HistogramSample
    { histBuckets :: Buckets
    , histSum     :: Double
    , histCount   :: Count
    }


new :: [UpperBound] -> IO Histogram
new buckets = Histogram <$> newIORef empty
  where empty = HistogramSample (Map.fromList $ map (, 0) (read "Infinity" : buckets)) 0.0 0


put :: Double -> Histogram -> IO ()
put x ioref = atomicModifyIORef' (unHistogram ioref) update
    where update histData = (hist' histData, ())
          hist' histData =
              histData { histBuckets = Map.adjust (+ 1) (getBucket x (Map.keys $ histBuckets histData)) (histBuckets histData)
                       , histSum = histSum histData + x
                       , histCount = histCount histData + 1
                       }


getBucket :: Double -> [UpperBound] -> Double
getBucket x buckets = foldl' f (head buckets) buckets
  where
    f prev curr
        | x <= curr && x > prev = curr
        | otherwise = prev


sample :: Histogram -> IO HistogramSample
sample = readIORef . unHistogram
