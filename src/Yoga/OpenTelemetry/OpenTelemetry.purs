module Yoga.OpenTelemetry.OpenTelemetry where

import Prelude

import Data.DateTime.Instant (Instant)
import Data.Int (toNumber)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row (class Union)
import Promise (Promise)
import Promise.Aff (toAffE) as Promise
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)

-- Opaque OpenTelemetry types
foreign import data TracerProvider :: Type
foreign import data Tracer :: Type
foreign import data Span :: Type
foreign import data SpanContext :: Type

-- Newtypes for type safety

-- Service and resource identifiers
newtype ServiceName = ServiceName String
derive instance Newtype ServiceName _
derive newtype instance Eq ServiceName
derive newtype instance Show ServiceName

newtype ServiceVersion = ServiceVersion String
derive instance Newtype ServiceVersion _
derive newtype instance Eq ServiceVersion
derive newtype instance Show ServiceVersion

newtype ServiceNamespace = ServiceNamespace String
derive instance Newtype ServiceNamespace _
derive newtype instance Eq ServiceNamespace
derive newtype instance Show ServiceNamespace

newtype InstrumentationName = InstrumentationName String
derive instance Newtype InstrumentationName _
derive newtype instance Eq InstrumentationName
derive newtype instance Show InstrumentationName

newtype InstrumentationVersion = InstrumentationVersion String
derive instance Newtype InstrumentationVersion _
derive newtype instance Eq InstrumentationVersion
derive newtype instance Show InstrumentationVersion

newtype TracerName = TracerName String
derive instance Newtype TracerName _
derive newtype instance Eq TracerName
derive newtype instance Show TracerName

newtype SpanName = SpanName String
derive instance Newtype SpanName _
derive newtype instance Eq SpanName
derive newtype instance Show SpanName

-- Attribute value types (similar to tags but OpenTelemetry naming)
data AttributeValue
  = AttrString String
  | AttrNumber Number
  | AttrBoolean Boolean
  | AttrArrayString (Array String)
  | AttrArrayNumber (Array Number)
  | AttrArrayBoolean (Array Boolean)

derive instance Eq AttributeValue
instance Show AttributeValue where
  show (AttrString s) = "AttrString " <> show s
  show (AttrNumber n) = "AttrNumber " <> show n
  show (AttrBoolean b) = "AttrBoolean " <> show b
  show (AttrArrayString arr) = "AttrArrayString " <> show arr
  show (AttrArrayNumber arr) = "AttrArrayNumber " <> show arr
  show (AttrArrayBoolean arr) = "AttrArrayBoolean " <> show arr

-- Span kind (OpenTelemetry span types)
data SpanKind 
  = SpanKindInternal
  | SpanKindServer
  | SpanKindClient
  | SpanKindProducer
  | SpanKindConsumer

derive instance Eq SpanKind
derive instance Ord SpanKind

instance Show SpanKind where
  show SpanKindInternal = "INTERNAL"
  show SpanKindServer = "SERVER"
  show SpanKindClient = "CLIENT"
  show SpanKindProducer = "PRODUCER"
  show SpanKindConsumer = "CONSUMER"

-- Span status
data SpanStatusCode
  = StatusUnset
  | StatusOk
  | StatusError

derive instance Eq SpanStatusCode
derive instance Ord SpanStatusCode

instance Show SpanStatusCode where
  show StatusUnset = "UNSET"
  show StatusOk = "OK"
  show StatusError = "ERROR"

-- Span reference types (for links)
data SpanLinkType = ParentLink | PreviousLink

derive instance Eq SpanLinkType
derive instance Ord SpanLinkType

-- Configuration types

-- Resource attributes (service metadata)
type ResourceAttributesImpl =
  ( serviceName :: ServiceName
  , serviceVersion :: ServiceVersion
  , serviceNamespace :: ServiceNamespace
  )

-- Tracer provider configuration (exporter is passed as opaque object)
type TracerProviderConfigImpl resource =
  ( resource :: { | resource }
  , exporter :: SpanExporter
  )

-- Opaque exporter type (implemented by exporter modules)
foreign import data SpanExporter :: Type

-- Initialize tracer provider
foreign import createTracerProviderImpl :: forall opts. EffectFn1 { | opts } TracerProvider

createTracerProvider :: forall opts opts_ resource. Union opts opts_ (TracerProviderConfigImpl resource) => { | opts } -> Effect TracerProvider
createTracerProvider opts = runEffectFn1 createTracerProviderImpl opts

-- Get tracer from provider
foreign import getTracerImpl :: EffectFn3 TracerProvider TracerName String Tracer

getTracer :: TracerName -> TracerProvider -> Effect Tracer
getTracer name provider = runEffectFn3 getTracerImpl provider name ""

getTracerWithVersion :: TracerName -> InstrumentationVersion -> TracerProvider -> Effect Tracer
getTracerWithVersion name (InstrumentationVersion version) provider = 
  runEffectFn3 getTracerImpl provider name version

-- Span options
type StartSpanOptionsImpl =
  ( kind :: SpanKind
  , attributes :: { | AttributesRow }
  , links :: Array SpanLink
  , startTime :: Instant
  , parent :: SpanContext
  )

type AttributesRow :: Row Type
type AttributesRow = ()

-- Span link for relating spans
type SpanLink =
  { context :: SpanContext
  , attributes :: Object AttributeValue
  }

-- Start span
foreign import startSpanImpl :: forall opts. EffectFn3 Tracer SpanName { | opts } Span

startSpan :: SpanName -> Tracer -> Effect Span
startSpan name tracer = runEffectFn3 startSpanImpl tracer name {}

startSpanWithOptions :: forall opts opts_. Union opts opts_ StartSpanOptionsImpl => SpanName -> { | opts } -> Tracer -> Effect Span
startSpanWithOptions name opts tracer = runEffectFn3 startSpanImpl tracer name opts

-- Helper for starting span with attributes (most common case)
startSpanWithAttributes :: forall r. Homogeneous r AttributeValue => SpanName -> { | r } -> Tracer -> Effect Span
startSpanWithAttributes name attrs tracer = 
  runEffectFn3 startSpanImpl tracer name { attributes: unsafeCoerce (Object.fromHomogeneous attrs) }

-- Start span with kind and attributes
startSpanWithKind :: forall r. Homogeneous r AttributeValue => SpanName -> SpanKind -> { | r } -> Tracer -> Effect Span
startSpanWithKind name kind attrs tracer = 
  runEffectFn3 startSpanImpl tracer name 
    { kind: kind
    , attributes: unsafeCoerce (Object.fromHomogeneous attrs)
    }

-- Start active span (sets as current context)
foreign import startActiveSpanImpl :: forall opts a. EffectFn4 Tracer SpanName { | opts } (Span -> Effect a) a

startActiveSpan :: forall a. SpanName -> (Span -> Effect a) -> Tracer -> Effect a
startActiveSpan name fn tracer = runEffectFn4 startActiveSpanImpl tracer name {} fn

startActiveSpanWithOptions :: forall opts opts_ a. Union opts opts_ StartSpanOptionsImpl => SpanName -> { | opts } -> (Span -> Effect a) -> Tracer -> Effect a
startActiveSpanWithOptions name opts fn tracer = runEffectFn4 startActiveSpanImpl tracer name opts fn

-- Get span context
foreign import getSpanContextImpl :: EffectFn1 Span SpanContext

getSpanContext :: Span -> Effect SpanContext
getSpanContext span = runEffectFn1 getSpanContextImpl span

-- Set attribute
foreign import setAttributeImpl :: EffectFn3 Span String AttributeValue Unit

setAttribute :: String -> AttributeValue -> Span -> Effect Unit
setAttribute key value span = runEffectFn3 setAttributeImpl span key value

-- Set multiple attributes
foreign import setAttributesImpl :: EffectFn2 Span (Object AttributeValue) Unit

setAttributes :: forall r. Homogeneous r AttributeValue => { | r } -> Span -> Effect Unit
setAttributes attrs span = runEffectFn2 setAttributesImpl span (Object.fromHomogeneous attrs)

-- Add event (structured log)
foreign import addEventImpl :: EffectFn2 Span String Unit

addEvent :: String -> Span -> Effect Unit
addEvent name span = runEffectFn2 addEventImpl span name

-- Add event with attributes
foreign import addEventWithAttributesImpl :: EffectFn3 Span String (Object AttributeValue) Unit

addEventWithAttributes :: forall r. Homogeneous r AttributeValue => String -> { | r } -> Span -> Effect Unit
addEventWithAttributes name attrs span = 
  runEffectFn3 addEventWithAttributesImpl span name (Object.fromHomogeneous attrs)

-- Add event with timestamp
foreign import addEventWithTimestampImpl :: EffectFn4 Span String (Object AttributeValue) Instant Unit

addEventWithTimestamp :: forall r. Homogeneous r AttributeValue => String -> { | r } -> Instant -> Span -> Effect Unit
addEventWithTimestamp name attrs timestamp span = 
  runEffectFn4 addEventWithTimestampImpl span name (Object.fromHomogeneous attrs) timestamp

-- Set span status
foreign import setStatusImpl :: EffectFn2 Span SpanStatusCode Unit

setStatus :: SpanStatusCode -> Span -> Effect Unit
setStatus status span = runEffectFn2 setStatusImpl span status

-- Set span status with message
foreign import setStatusWithMessageImpl :: EffectFn3 Span SpanStatusCode String Unit

setStatusWithMessage :: SpanStatusCode -> String -> Span -> Effect Unit
setStatusWithMessage status message span = runEffectFn3 setStatusWithMessageImpl span status message

-- Record exception
foreign import recordExceptionImpl :: EffectFn2 Span String Unit

recordException :: String -> Span -> Effect Unit
recordException error span = runEffectFn2 recordExceptionImpl span error

-- Record exception with timestamp
foreign import recordExceptionWithTimestampImpl :: EffectFn3 Span String Instant Unit

recordExceptionWithTimestamp :: String -> Instant -> Span -> Effect Unit
recordExceptionWithTimestamp error timestamp span = 
  runEffectFn3 recordExceptionWithTimestampImpl span error timestamp

-- Update span name
foreign import updateNameImpl :: EffectFn2 Span SpanName Unit

updateName :: SpanName -> Span -> Effect Unit
updateName name span = runEffectFn2 updateNameImpl span name

-- End span
foreign import endSpanImpl :: EffectFn1 Span Unit

endSpan :: Span -> Effect Unit
endSpan span = runEffectFn1 endSpanImpl span

-- End span with timestamp
foreign import endSpanWithTimestampImpl :: EffectFn2 Span Instant Unit

endSpanWithTimestamp :: Instant -> Span -> Effect Unit
endSpanWithTimestamp timestamp span = runEffectFn2 endSpanWithTimestampImpl span timestamp

-- Check if span is recording
foreign import isRecordingImpl :: EffectFn1 Span Boolean

isRecording :: Span -> Effect Boolean
isRecording span = runEffectFn1 isRecordingImpl span

-- Shutdown tracer provider (flush and close)
foreign import shutdownImpl :: EffectFn1 TracerProvider (Promise Unit)

shutdown :: TracerProvider -> Aff Unit
shutdown provider = runEffectFn1 shutdownImpl provider # Promise.toAffE

-- Force flush
foreign import forceFlushImpl :: EffectFn1 TracerProvider (Promise Unit)

forceFlush :: TracerProvider -> Aff Unit
forceFlush provider = runEffectFn1 forceFlushImpl provider # Promise.toAffE

-- Helper functions for common span operations

-- Wrap an Effect action with a span
withSpan :: forall a. SpanName -> Tracer -> Effect a -> Effect a
withSpan name tracer action = do
  span <- startSpan name tracer
  result <- action
  endSpan span
  pure result

-- Wrap an Aff action with a span
withSpanAff :: forall a. SpanName -> Tracer -> Aff a -> Aff a
withSpanAff name tracer action = do
  span <- liftEffect $ startSpan name tracer
  result <- action
  liftEffect $ endSpan span
  pure result

-- Wrap with error handling
withSpanCatch :: forall a. SpanName -> Tracer -> Effect a -> Effect a
withSpanCatch name tracer action = do
  span <- startSpan name tracer
  result <- action
  setStatus StatusOk span
  endSpan span
  pure result

withSpanAffCatch :: forall a. SpanName -> Tracer -> Aff a -> Aff a
withSpanAffCatch name tracer action = do
  span <- liftEffect $ startSpan name tracer
  result <- action
  liftEffect $ setStatus StatusOk span
  liftEffect $ endSpan span
  pure result

-- Semantic convention helpers (common attributes)

newtype HTTPMethod = HTTPMethod String
derive instance Newtype HTTPMethod _
derive newtype instance Eq HTTPMethod
derive newtype instance Show HTTPMethod

setHTTPMethod :: HTTPMethod -> Span -> Effect Unit
setHTTPMethod (HTTPMethod method) span = setAttribute "http.method" (AttrString method) span

newtype HTTPURL = HTTPURL String
derive instance Newtype HTTPURL _
derive newtype instance Eq HTTPURL
derive newtype instance Show HTTPURL

setHTTPURL :: HTTPURL -> Span -> Effect Unit
setHTTPURL (HTTPURL url) span = setAttribute "http.url" (AttrString url) span

newtype HTTPTarget = HTTPTarget String
derive instance Newtype HTTPTarget _
derive newtype instance Eq HTTPTarget
derive newtype instance Show HTTPTarget

setHTTPTarget :: HTTPTarget -> Span -> Effect Unit
setHTTPTarget (HTTPTarget target) span = setAttribute "http.target" (AttrString target) span

newtype HTTPStatusCode = HTTPStatusCode Int
derive instance Newtype HTTPStatusCode _
derive newtype instance Eq HTTPStatusCode
derive newtype instance Ord HTTPStatusCode
derive newtype instance Show HTTPStatusCode

setHTTPStatusCode :: HTTPStatusCode -> Span -> Effect Unit
setHTTPStatusCode (HTTPStatusCode code) span = setAttribute "http.status_code" (AttrNumber $ toNumber code) span

newtype HTTPScheme = HTTPScheme String
derive instance Newtype HTTPScheme _
derive newtype instance Eq HTTPScheme
derive newtype instance Show HTTPScheme

setHTTPScheme :: HTTPScheme -> Span -> Effect Unit
setHTTPScheme (HTTPScheme scheme) span = setAttribute "http.scheme" (AttrString scheme) span

newtype HTTPHost = HTTPHost String
derive instance Newtype HTTPHost _
derive newtype instance Eq HTTPHost
derive newtype instance Show HTTPHost

setHTTPHost :: HTTPHost -> Span -> Effect Unit
setHTTPHost (HTTPHost host) span = setAttribute "http.host" (AttrString host) span

newtype DBSystem = DBSystem String
derive instance Newtype DBSystem _
derive newtype instance Eq DBSystem
derive newtype instance Show DBSystem

setDBSystem :: DBSystem -> Span -> Effect Unit
setDBSystem (DBSystem system) span = setAttribute "db.system" (AttrString system) span

newtype DBStatement = DBStatement String
derive instance Newtype DBStatement _
derive newtype instance Eq DBStatement
derive newtype instance Show DBStatement

setDBStatement :: DBStatement -> Span -> Effect Unit
setDBStatement (DBStatement statement) span = setAttribute "db.statement" (AttrString statement) span

newtype DBName = DBName String
derive instance Newtype DBName _
derive newtype instance Eq DBName
derive newtype instance Show DBName

setDBName :: DBName -> Span -> Effect Unit
setDBName (DBName name) span = setAttribute "db.name" (AttrString name) span

newtype PeerService = PeerService String
derive instance Newtype PeerService _
derive newtype instance Eq PeerService
derive newtype instance Show PeerService

setPeerService :: PeerService -> Span -> Effect Unit
setPeerService (PeerService service) span = setAttribute "peer.service" (AttrString service) span
