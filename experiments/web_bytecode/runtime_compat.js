// OxCaml's browser runtime is derived from the 5.2 runtime, while the current
// compiler identifies itself as 5.4. Keep primitives whose implementation did
// not change available to js_of_ocaml's linker under the current version.

//Provides: caml_hash_exn
//Requires: caml_hash
var caml_hash_exn = caml_hash;

// The toplevel resolves primitives in dynamically loaded Dox bytecode through
// js_of_ocaml's runtime table. These wrappers keep the implementation in the
// separately loaded shim while making every primitive visible to that table.

//Provides: doxSetEnv
function doxSetEnv(name, value) { return globalThis.doxSetEnv(name, value); }

//Provides: doxResetTrace
function doxResetTrace(unit) { globalThis.doxResetTrace(); return 0; }

//Provides: doxReadTrace
function doxReadTrace(unit) { return globalThis.doxReadTrace(); }

//Provides: caml_doclang_observe_enter
function caml_doclang_observe_enter(metadata) { return globalThis.caml_doclang_observe_enter(metadata); }

//Provides: caml_doclang_observe_enter_tail
function caml_doclang_observe_enter_tail(metadata) { return globalThis.caml_doclang_observe_enter_tail(metadata); }

//Provides: caml_doclang_observe_parameter
function caml_doclang_observe_parameter(occurrence, metadata, observed) { return globalThis.caml_doclang_observe_parameter(occurrence, metadata, observed); }

//Provides: caml_doclang_observe_write
function caml_doclang_observe_write(metadata, observed) { return globalThis.caml_doclang_observe_write(metadata, observed); }

//Provides: caml_doclang_observe_return
function caml_doclang_observe_return(metadata, occurrence, observed) { return globalThis.caml_doclang_observe_return(metadata, occurrence, observed); }

//Provides: caml_doclang_observe_raise
function caml_doclang_observe_raise(metadata, occurrence, observed) { return globalThis.caml_doclang_observe_raise(metadata, occurrence, observed); }

//Provides: caml_doclang_observe_register_function
function caml_doclang_observe_register_function(fn, consumption, metadata) { return globalThis.caml_doclang_observe_register_function(fn, consumption, metadata); }

//Provides: caml_doclang_observe_register_partial
function caml_doclang_observe_register_partial(original, partial) { return globalThis.caml_doclang_observe_register_partial(original, partial); }

//Provides: caml_doclang_observe_call_function
function caml_doclang_observe_call_function(fn) { return globalThis.caml_doclang_observe_call_function(fn); }

//Provides: caml_doclang_observe_is_registered_function
function caml_doclang_observe_is_registered_function(fn, supplied) { return globalThis.caml_doclang_observe_is_registered_function(fn, supplied); }

//Provides: caml_doclang_observe_tail_handoff
function caml_doclang_observe_tail_handoff(metadata, occurrence, fn, supplied) { return globalThis.caml_doclang_observe_tail_handoff(metadata, occurrence, fn, supplied); }
