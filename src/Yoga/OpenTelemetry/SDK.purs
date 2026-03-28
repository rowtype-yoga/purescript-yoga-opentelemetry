module Yoga.OpenTelemetry.SDK
  ( NodeSDK
  , initializeOpenTelemetrySDK
  , shutdownSDK
  , getTracerFromSDK
  ) where

import Data.Function ((#))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Promise (Promise)
import Promise.Aff as Promise
import Yoga.OpenTelemetry.OpenTelemetry (ServiceName(..), ServiceVersion(..), ServiceNamespace(..), Tracer, TracerName(..))

-- Opaque SDK type
foreign import data NodeSDK :: Type

-- FFI imports
foreign import initializeOpenTelemetrySDKImpl
  :: EffectFn1
       { serviceName :: String
       , serviceVersion :: String
       , serviceNamespace :: String
       , logsEndpoint :: String
       , tracesEndpoint :: String
       }
       NodeSDK

foreign import getTracerImpl :: EffectFn1 String Tracer
foreign import shutdownSDKImpl :: EffectFn1 NodeSDK (Promise Unit)

-- Public API
initializeOpenTelemetrySDK
  :: { serviceName :: ServiceName
     , serviceVersion :: ServiceVersion
     , serviceNamespace :: ServiceNamespace
     , logsEndpoint :: String
     , tracesEndpoint :: String
     }
  -> Effect NodeSDK
initializeOpenTelemetrySDK { serviceName: ServiceName sn, serviceVersion: ServiceVersion sv, serviceNamespace: ServiceNamespace sns, logsEndpoint, tracesEndpoint } =
  runEffectFn1 initializeOpenTelemetrySDKImpl
    { serviceName: sn
    , serviceVersion: sv
    , serviceNamespace: sns
    , logsEndpoint
    , tracesEndpoint
    }

getTracerFromSDK :: TracerName -> Effect Tracer
getTracerFromSDK (TracerName name) = runEffectFn1 getTracerImpl name

shutdownSDK :: NodeSDK -> Aff Unit
shutdownSDK sdk = runEffectFn1 shutdownSDKImpl sdk # Promise.toAffE
