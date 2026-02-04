const { NodeTracerProvider } = require('@opentelemetry/sdk-trace-node');
const { Resource } = require('@opentelemetry/resources');
const { ATTR_SERVICE_NAME, ATTR_SERVICE_VERSION } = require('@opentelemetry/semantic-conventions');
const { BatchSpanProcessor } = require('@opentelemetry/sdk-trace-base');
const { SpanKind, SpanStatusCode, trace, context } = require('@opentelemetry/api');

// Convert AttributeValue ADT to JS value
const attributeValueToJS = (attrValue) => {
  const constructorName = attrValue.constructor.name;
  
  if (constructorName === 'AttrString') {
    return attrValue.value0;
  } else if (constructorName === 'AttrNumber') {
    return attrValue.value0;
  } else if (constructorName === 'AttrBoolean') {
    return attrValue.value0;
  } else if (constructorName === 'AttrArrayString') {
    return attrValue.value0;
  } else if (constructorName === 'AttrArrayNumber') {
    return attrValue.value0;
  } else if (constructorName === 'AttrArrayBoolean') {
    return attrValue.value0;
  }
  
  return attrValue; // fallback
};

// Convert SpanKind ADT to OpenTelemetry SpanKind
const spanKindToJS = (kind) => {
  const constructorName = kind.constructor.name;
  
  switch (constructorName) {
    case 'SpanKindInternal':
      return SpanKind.INTERNAL;
    case 'SpanKindServer':
      return SpanKind.SERVER;
    case 'SpanKindClient':
      return SpanKind.CLIENT;
    case 'SpanKindProducer':
      return SpanKind.PRODUCER;
    case 'SpanKindConsumer':
      return SpanKind.CONSUMER;
    default:
      return SpanKind.INTERNAL;
  }
};

// Convert SpanStatusCode ADT to OpenTelemetry StatusCode
const statusCodeToJS = (status) => {
  const constructorName = status.constructor.name;
  
  switch (constructorName) {
    case 'StatusUnset':
      return SpanStatusCode.UNSET;
    case 'StatusOk':
      return SpanStatusCode.OK;
    case 'StatusError':
      return SpanStatusCode.ERROR;
    default:
      return SpanStatusCode.UNSET;
  }
};

// Convert Object AttributeValue to plain JS object
const convertAttributes = (attrs) => {
  const jsAttrs = {};
  for (const [key, attrValue] of Object.entries(attrs)) {
    jsAttrs[key] = attributeValueToJS(attrValue);
  }
  return jsAttrs;
};

// Create tracer provider
export const createTracerProviderImpl = (config) => {
  // Build resource attributes
  const resourceAttrs = {};
  
  if (config.resource) {
    if (config.resource.serviceName) {
      resourceAttrs[ATTR_SERVICE_NAME] = config.resource.serviceName;
    }
    if (config.resource.serviceVersion) {
      resourceAttrs[ATTR_SERVICE_VERSION] = config.resource.serviceVersion;
    }
    if (config.resource.serviceNamespace) {
      resourceAttrs['service.namespace'] = config.resource.serviceNamespace;
    }
  }
  
  const resource = Resource.default().merge(
    new Resource(resourceAttrs)
  );
  
  const provider = new NodeTracerProvider({
    resource: resource,
  });
  
  // Exporter is passed as an opaque object from exporter modules
  if (config.exporter) {
    provider.addSpanProcessor(new BatchSpanProcessor(config.exporter));
  }
  
  // Register the provider
  provider.register();
  
  return provider;
};

// Get tracer from provider
export const getTracerImpl = (provider, name, version) => {
  return provider.getTracer(name, version || undefined);
};

// Start span
export const startSpanImpl = (tracer, name, options) => {
  const spanOptions = {};
  
  // Handle span kind
  if (options.kind) {
    spanOptions.kind = spanKindToJS(options.kind);
  }
  
  // Handle attributes
  if (options.attributes) {
    spanOptions.attributes = convertAttributes(options.attributes);
  }
  
  // Handle links
  if (options.links && Array.isArray(options.links)) {
    spanOptions.links = options.links.map(link => ({
      context: link.context,
      attributes: convertAttributes(link.attributes || {})
    }));
  }
  
  // Handle start time (Instant is milliseconds since epoch)
  if (options.startTime !== undefined) {
    spanOptions.startTime = options.startTime;
  }
  
  // Handle parent context
  if (options.parent) {
    // Create a new context with the parent span
    const parentContext = trace.setSpanContext(context.active(), options.parent);
    return tracer.startSpan(name, spanOptions, parentContext);
  }
  
  return tracer.startSpan(name, spanOptions);
};

// Start active span (sets as current context)
export const startActiveSpanImpl = (tracer, name, options, fn) => {
  const spanOptions = {};
  
  // Handle span kind
  if (options.kind) {
    spanOptions.kind = spanKindToJS(options.kind);
  }
  
  // Handle attributes
  if (options.attributes) {
    spanOptions.attributes = convertAttributes(options.attributes);
  }
  
  // Handle links
  if (options.links && Array.isArray(options.links)) {
    spanOptions.links = options.links.map(link => ({
      context: link.context,
      attributes: convertAttributes(link.attributes || {})
    }));
  }
  
  // Handle start time
  if (options.startTime !== undefined) {
    spanOptions.startTime = options.startTime;
  }
  
  return tracer.startActiveSpan(name, spanOptions, (span) => {
    try {
      // fn is (Span -> Effect a), calling it gives us Effect a
      // Effect a is a thunk, so we need to call it
      const effectAction = fn(span);
      const result = effectAction(); // Execute the Effect
      return result;
    } finally {
      // Span is automatically ended by OpenTelemetry when the function completes
    }
  });
};

// Get span context
export const getSpanContextImpl = (span) => {
  return span.spanContext();
};

// Set attribute
export const setAttributeImpl = (span, key, attrValue) => {
  span.setAttribute(key, attributeValueToJS(attrValue));
};

// Set multiple attributes
export const setAttributesImpl = (span, attrs) => {
  span.setAttributes(convertAttributes(attrs));
};

// Add event (structured log)
export const addEventImpl = (span, name) => {
  span.addEvent(name);
};

// Add event with attributes
export const addEventWithAttributesImpl = (span, name, attrs) => {
  span.addEvent(name, convertAttributes(attrs));
};

// Add event with timestamp
export const addEventWithTimestampImpl = (span, name, attrs, timestamp) => {
  span.addEvent(name, convertAttributes(attrs), timestamp);
};

// Set span status
export const setStatusImpl = (span, statusCode) => {
  span.setStatus({
    code: statusCodeToJS(statusCode)
  });
};

// Set span status with message
export const setStatusWithMessageImpl = (span, statusCode, message) => {
  span.setStatus({
    code: statusCodeToJS(statusCode),
    message: message
  });
};

// Record exception
export const recordExceptionImpl = (span, error) => {
  // If error is a string, create an Error object
  const errorObj = typeof error === 'string' ? new Error(error) : error;
  span.recordException(errorObj);
};

// Record exception with timestamp
export const recordExceptionWithTimestampImpl = (span, error, timestamp) => {
  const errorObj = typeof error === 'string' ? new Error(error) : error;
  span.recordException(errorObj, timestamp);
};

// Update span name
export const updateNameImpl = (span, name) => {
  span.updateName(name);
};

// End span
export const endSpanImpl = (span) => {
  span.end();
};

// End span with timestamp
export const endSpanWithTimestampImpl = (span, timestamp) => {
  span.end(timestamp);
};

// Check if span is recording
export const isRecordingImpl = (span) => {
  return span.isRecording();
};

// Shutdown tracer provider
export const shutdownImpl = (provider) => {
  return provider.shutdown();
};

// Force flush
export const forceFlushImpl = (provider) => {
  return provider.forceFlush();
};
