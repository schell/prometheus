{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Metrics.Prometheus.RegistryT where

import           Control.Monad.IO.Class                     (MonadIO)
import           Control.Monad.State.Class                  (MonadState, get)
import           Control.Monad.Trans                        (lift)
import           Control.Monad.Trans.Class                  (MonadTrans)
import           Control.Monad.Trans.State.Strict           (StateT (..),
                                                             evalStateT,
                                                             execStateT)

import           System.Metrics.Prometheus.Metric.Counter   (Counter)
import           System.Metrics.Prometheus.Metric.Gauge     (Gauge)
import           System.Metrics.Prometheus.Metric.Histogram (Histogram)
import qualified System.Metrics.Prometheus.Metric.Histogram as Histogram
import           System.Metrics.Prometheus.MetricId         (Labels, Name)
import           System.Metrics.Prometheus.Registry         (Registry (..), new)
import qualified System.Metrics.Prometheus.Registry         as R


newtype RegistryT m a =
    RegistryT { unRegistryT :: StateT Registry m a }
    deriving ( Monad, MonadTrans, Applicative, Functor
             , MonadState Registry, MonadIO)


evalRegistryT :: Monad m => RegistryT m a -> m a
evalRegistryT registry = evalStateT (unRegistryT registry) new


execRegistryT :: Monad m => RegistryT m a -> m Registry
execRegistryT registry = execStateT (unRegistryT registry) new


runRegistryT :: Monad m => RegistryT m a -> m (a, Registry)
runRegistryT registry = runStateT (unRegistryT registry) new


withRegistry :: (Registry -> m (a, Registry)) -> RegistryT m a
withRegistry = RegistryT . StateT


registerCounter :: Name -> Labels -> RegistryT IO Counter
registerCounter = (.) withRegistry . R.registerCounter


registerGauge :: Name -> Labels -> RegistryT IO Gauge
registerGauge = (.) withRegistry . R.registerGauge


registerHistogram :: Name -> Labels -> [Histogram.UpperBound] -> RegistryT IO Histogram
registerHistogram = (.) (withRegistry .) . R.registerHistogram


sample :: RegistryT IO R.RegistrySample
sample = get >>= (lift . R.sample)
