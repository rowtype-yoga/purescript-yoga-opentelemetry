module Yoga.OpenTelemetry.Logs
  ( LoggerProvider
  , OTLPLogsEndpoint(..)
  , createLoggerProvider
  , registerLoggerProvider
  , createPinoInstrumentation
  , PinoInstrumentation
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Yoga.OpenTelemetry.OpenTelemetry (ServiceName(..), ServiceVersion(..), ServiceNamespace(..))

-- Opaque types
foreign import data LoggerProvider :: Type
foreign import data PinoInstrumentation :: Type

newtype OTLPLogsEndpoint = OTLPLogsEndpoint String

derive newtype instance Eq OTLPLogsEndpoint
derive newtype instance Show OTLPLogsEndpoint

-- FFI imports
foreign import createLoggerProviderImpl
  :: EffectFn1
       { resource ::
           { serviceName :: String
           , serviceVersion :: String
           , serviceNamespace :: String
           }
       , exporter ::
           { url :: String
           }
       }
       LoggerProvider

foreign import registerLoggerProviderImpl :: EffectFn1 LoggerProvider Unit

foreign import createPinoInstrumentationImpl :: Effect PinoInstrumentation

-- Public API
createLoggerProvider
  :: { serviceName :: ServiceName
     , serviceVersion :: ServiceVersion
     , serviceNamespace :: ServiceNamespace
     , endpoint :: OTLPLogsEndpoint
     }
  -> Effect LoggerProvider
createLoggerProvider { serviceName: ServiceName sn, serviceVersion: ServiceVersion sv, serviceNamespace: ServiceNamespace sns, endpoint: OTLPLogsEndpoint url } =
  runEffectFn1 createLoggerProviderImpl
    { resource: { serviceName: sn, serviceVersion: sv, serviceNamespace: sns }
    , exporter: { url }
    }

registerLoggerProvider :: LoggerProvider -> Effect Unit
registerLoggerProvider = runEffectFn1 registerLoggerProviderImpl

createPinoInstrumentation :: Effect PinoInstrumentation
createPinoInstrumentation = createPinoInstrumentationImpl
