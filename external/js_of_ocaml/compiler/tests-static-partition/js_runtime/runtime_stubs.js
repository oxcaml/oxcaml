//Provides: runtime_probe_state
var runtime_probe_state = { count: 0 };

//Provides: runtime_probe_transitive
//Requires: runtime_probe_state
function runtime_probe_transitive() { return runtime_probe_state; }

//Provides: runtime_probe_main
//Requires: runtime_probe_transitive
function runtime_probe_main() { return runtime_probe_transitive(); }

//Provides: runtime_probe_lazy
//Alias: runtime_probe_alias
//Requires: runtime_probe_transitive
function runtime_probe_lazy() { return runtime_probe_transitive(); }

//Provides: runtime_probe_unused
function runtime_probe_unused() { throw new Error("UNUSED_RUNTIME_PRIMITIVE_SENTINEL"); }
