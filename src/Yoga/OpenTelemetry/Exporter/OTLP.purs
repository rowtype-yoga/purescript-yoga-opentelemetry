module Yoga.OpenTelemetry.Exporter.OTLP where

import Prelude

import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Yoga.OpenTelemetry.OpenTelemetry (SpanExporter)
import Prim.Row (class Union)

-- OTLP-specific types
newtype OTLPEndpoint = OTLPEndpoint String

derive instance Newtype OTLPEndpoint _
derive newtype instance Eq OTLPEndpoint
derive newtype instance Show OTLPEndpoint

-- OTLP exporter configuration
type OTLPExporterConfigImpl =
  ( url :: OTLPEndpoint
  )

-- Create OTLP exporter
foreign import createOTLPExporterImpl :: forall opts. EffectFn1 { | opts } SpanExporter

createOTLPExporter :: forall opts opts_. Union opts opts_ OTLPExporterConfigImpl => { | opts } -> Effect SpanExporter
createOTLPExporter opts = runEffectFn1 createOTLPExporterImpl opts

-- Convenience function with defaults for local Jaeger
-- Jaeger's OTLP HTTP endpoint is at port 4318
createOTLPExporterDefaults :: Effect SpanExporter
createOTLPExporterDefaults = createOTLPExporter
  { url: OTLPEndpoint "http://localhost:4318/v1/traces" }
