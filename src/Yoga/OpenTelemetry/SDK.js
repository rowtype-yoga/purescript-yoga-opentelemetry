import { NodeSDK } from '@opentelemetry/sdk-node';
import { OTLPLogExporter } from '@opentelemetry/exporter-logs-otlp-http';
import { OTLPTraceExporter } from '@opentelemetry/exporter-trace-otlp-http';
import { PinoInstrumentation } from '@opentelemetry/instrumentation-pino';
import { Resource } from '@opentelemetry/resources';
import { ATTR_SERVICE_NAME, ATTR_SERVICE_VERSION } from '@opentelemetry/semantic-conventions';
import { BatchLogRecordProcessor } from '@opentelemetry/sdk-logs';
import { BatchSpanProcessor } from '@opentelemetry/sdk-trace-base';

/**
 * Initialize OpenTelemetry SDK with logs and traces
 * This MUST be called before creating any Pino loggers!
 */
export const initializeOpenTelemetrySDKImpl = (config) => {
  const resource = new Resource({
    [ATTR_SERVICE_NAME]: config.serviceName,
    [ATTR_SERVICE_VERSION]: config.serviceVersion,
    'service.namespace': config.serviceNamespace,
  });

  const logExporter = new OTLPLogExporter({
    url: config.logsEndpoint,
  });

  const traceExporter = new OTLPTraceExporter({
    url: config.tracesEndpoint,
  });

  const sdk = new NodeSDK({
    resource,
    logRecordProcessor: new BatchLogRecordProcessor(logExporter),
    spanProcessor: new BatchSpanProcessor(traceExporter),
    instrumentations: [
      new PinoInstrumentation({
        // Optional: add log-trace correlation
        logHook: (span, record) => {
          if (span) {
            record['trace_id'] = span.spanContext().traceId;
            record['span_id'] = span.spanContext().spanId;
            record['trace_flags'] = span.spanContext().traceFlags;
          }
        },
      }),
    ],
  });

  sdk.start();

  // Graceful shutdown
  process.on('SIGTERM', () => {
    sdk
      .shutdown()
      .then(() => console.log('OpenTelemetry SDK shut down successfully'))
      .catch((error) => console.error('Error shutting down SDK', error))
      .finally(() => process.exit(0));
  });

  return sdk;
};

/**
 * Get tracer from the global tracer provider (after SDK is initialized)
 */
export const getTracerImpl = (name) => {
  const api = require('@opentelemetry/api');
  return api.trace.getTracer(name);
};

/**
 * Register the logger provider globally so we can access it via the Logs API
 * The SDK initializes it, but we need to explicitly register it globally
 */
export const getLoggerProviderImpl = () => {
  const { logs } = require('@opentelemetry/api-logs');
  return logs.getLoggerProvider();
};
