const { ConsoleSpanExporter } = require('@opentelemetry/sdk-trace-base');

// Create console exporter
export const createConsoleExporter = () => {
  return new ConsoleSpanExporter();
};
