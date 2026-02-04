import { logs } from '@opentelemetry/api-logs';
import { LoggerProvider, BatchLogRecordProcessor } from '@opentelemetry/sdk-logs';
import { OTLPLogExporter } from '@opentelemetry/exporter-logs-otlp-http';
import { Resource } from '@opentelemetry/resources';
import { ATTR_SERVICE_NAME, ATTR_SERVICE_VERSION } from '@opentelemetry/semantic-conventions';
import { PinoInstrumentation } from '@opentelemetry/instrumentation-pino';

/**
 * Create a LoggerProvider with OTLP exporter
 * @param {Object} config - Configuration object
 * @param {Object} config.resource - Resource attributes
 * @param {string} config.resource.serviceName - Service name
 * @param {string} config.resource.serviceVersion - Service version  
 * @param {string} config.resource.serviceNamespace - Service namespace
 * @param {Object} config.exporter - Exporter configuration
 * @param {string} config.exporter.url - OTLP endpoint URL
 * @returns {LoggerProvider} The logger provider instance
 */
export const createLoggerProviderImpl = (config) => {
  const resource = new Resource({
    [ATTR_SERVICE_NAME]: config.resource.serviceName,
    [ATTR_SERVICE_VERSION]: config.resource.serviceVersion,
    'service.namespace': config.resource.serviceNamespace,  // Use string literal for namespace
  });

  const logExporter = new OTLPLogExporter({
    url: config.exporter.url,
  });

  const loggerProvider = new LoggerProvider({ 
    resource,
    logRecordProcessors: [new BatchLogRecordProcessor(logExporter)]
  });

  return loggerProvider;
};

/**
 * Register the logger provider globally
 * @param {LoggerProvider} provider - The logger provider to register
 * @returns {void}
 */
export const registerLoggerProviderImpl = (provider) => {
  logs.setGlobalLoggerProvider(provider);
};

/**
 * Create and enable Pino instrumentation to send logs to OpenTelemetry
 * This must be called BEFORE creating any Pino loggers
 * @returns {PinoInstrumentation} The instrumentation instance
 */
export const createPinoInstrumentationImpl = () => {
  const instrumentation = new PinoInstrumentation({
    logHook: (span, record, level) => {
      // Optionally correlate logs with active span
      if (span) {
        record['trace_id'] = span.spanContext().traceId;
        record['span_id'] = span.spanContext().spanId;
        record['trace_flags'] = span.spanContext().traceFlags;
      }
    },
  });
  
  instrumentation.enable();
  return instrumentation;
};

/**
 * Hook up an existing Pino logger to OpenTelemetry
 * Note: This is for already-created loggers. Ideally use instrumentation before logger creation.
 * @param {Object} logger - Pino logger instance
 * @returns {void}
 */
export const hookPinoToOtelImpl = (logger) => {
  // The instrumentation should handle this automatically when enabled before logger creation
  // This function exists for compatibility but the instrumentation approach is preferred
  console.warn('hookPinoToOtelImpl called - prefer enabling instrumentation before logger creation');
};
