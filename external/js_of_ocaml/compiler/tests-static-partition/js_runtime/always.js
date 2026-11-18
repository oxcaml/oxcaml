//Always
//Requires: runtime_probe_transitive
globalThis.runtimeProbeInitializations = (globalThis.runtimeProbeInitializations || 0) + 1;
globalThis.runtimeProbeAlways = runtime_probe_transitive();
