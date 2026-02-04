const { OTLPTraceExporter } = require('@opentelemetry/exporter-trace-otlp-http');

// Create OTLP exporter
export const createOTLPExporterImpl = (config) => {
  const otlpConfig = {};
  
  if (config.url) {
    otlpConfig.url = config.url;
  }
  
  return new OTLPTraceExporter(otlpConfig);
};
