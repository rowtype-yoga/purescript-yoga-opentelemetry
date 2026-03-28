module Yoga.OpenTelemetry.OmLayer
  ( OTelConfig
  , TracerL
  , otelLive
  , otelLive'
  ) where

import Prelude

import Effect.Class (liftEffect)
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, Scope, makeLayer, acquireRelease)
import Yoga.OpenTelemetry.OpenTelemetry as OTel
import Yoga.OpenTelemetry.SDK as OTelSDK

type OTelConfig =
  { serviceName :: OTel.ServiceName
  , serviceVersion :: OTel.ServiceVersion
  , serviceNamespace :: OTel.ServiceNamespace
  , logsEndpoint :: String
  , tracesEndpoint :: String
  , tracerName :: OTel.TracerName
  }

type TracerL r = (tracer :: OTel.Tracer | r)

otelLive :: forall r. OmLayer (scope :: Scope, otelConfig :: OTelConfig | r) () { tracer :: OTel.Tracer }
otelLive = makeLayer do
  { otelConfig } <- Om.ask
  acquireTracer otelConfig

otelLive' :: OTelConfig -> OmLayer (scope :: Scope) () { tracer :: OTel.Tracer }
otelLive' otelConfig = makeLayer do
  acquireTracer otelConfig

acquireTracer :: forall r. OTelConfig -> Om.Om { scope :: Scope | r } () { tracer :: OTel.Tracer }
acquireTracer config = do
  let { serviceName, serviceVersion, serviceNamespace, logsEndpoint, tracesEndpoint, tracerName } = config
  _ <- acquireRelease
    (OTelSDK.initializeOpenTelemetrySDK { serviceName, serviceVersion, serviceNamespace, logsEndpoint, tracesEndpoint } # liftEffect)
    OTelSDK.shutdownSDK
  tracer <- OTelSDK.getTracerFromSDK tracerName # liftEffect
  pure { tracer }
