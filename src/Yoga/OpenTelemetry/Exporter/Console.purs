module Yoga.OpenTelemetry.Exporter.Console where

import Effect (Effect)
import Yoga.OpenTelemetry.OpenTelemetry (SpanExporter)

-- Create console exporter (logs spans to console)
foreign import createConsoleExporter :: Effect SpanExporter
