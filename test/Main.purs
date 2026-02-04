module Test.Opentelemetry.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Yoga.OpenTelemetry.OpenTelemetry (ServiceName(..), ServiceVersion(..), ServiceNamespace(..))
import Yoga.OpenTelemetry.SDK (NodeSDK)
import Yoga.OpenTelemetry.SDK as SDK

spec :: Spec Unit
spec = do
  describe "Yoga.OpenTelemetry FFI" do
    describe "SDK Initialization" do
      it "creates NodeSDK instance" do
        _ <- liftEffect $ SDK.initializeOpenTelemetrySDK
          { serviceName: ServiceName "test"
          , serviceVersion: ServiceVersion "1.0.0"
          , serviceNamespace: ServiceNamespace "test"
          , logsEndpoint: "http://localhost:4318/v1/logs"
          , tracesEndpoint: "http://localhost:4318/v1/traces"
          }
        pure unit

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] spec
