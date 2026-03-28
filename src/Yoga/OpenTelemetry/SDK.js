import { NodeSDK } from '@opentelemetry/sdk-node';
import { OTLPLogExporter } from '@opentelemetry/exporter-logs-otlp-http';
import { OTLPTraceExporter } from '@opentelemetry/exporter-trace-otlp-http';
import { PinoInstrumentation } from '@opentelemetry/instrumentation-pino';
import { Resource } from '@opentelemetry/resources';
import { ATTR_SERVICE_NAME, ATTR_SERVICE_VERSION } from '@opentelemetry/semantic-conventions';
import { BatchLogRecordProcessor } from '@opentelemetry/sdk-logs';
import { BatchSpanProcessor } from '@opentelemetry/sdk-trace-base';
import api from '@opentelemetry/api';
import apiLogs from '@opentelemetry/api-logs';

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

  // Prevent the NodeSDK from auto-configuring a metric reader from env vars
  // (OTEL_METRICS_EXPORTER / OTEL_METRIC_EXPORT_INTERVAL) which can conflict.
  const savedMetricsExporter = process.env.OTEL_METRICS_EXPORTER;
  process.env.OTEL_METRICS_EXPORTER = 'none';

  const sdk = new NodeSDK({
    resource,
    logRecordProcessor: new BatchLogRecordProcessor(logExporter),
    spanProcessor: new BatchSpanProcessor(traceExporter),
    instrumentations: [
      new PinoInstrumentation({
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

  // Restore after start so we don't permanently clobber the env
  if (savedMetricsExporter !== undefined) {
    process.env.OTEL_METRICS_EXPORTER = savedMetricsExporter;
  } else {
    delete process.env.OTEL_METRICS_EXPORTER;
  }

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
  return api.trace.getTracer(name);
};

export const shutdownSDKImpl = (sdk) => {
  return sdk.shutdown();
};

/**
 * Register the logger provider globally so we can access it via the Logs API
 * The SDK initializes it, but we need to explicitly register it globally
 */
export const getLoggerProviderImpl = () => {
  return apiLogs.logs.getLoggerProvider();
};
