//Provides: ui64_ui64_make
//Requires: caml_js_from_array
function ui64_ui64_make() {
  return [0, [255, 123n], [255, 456n]];
}

//Provides: ui64_ui64_make_bytecode
//Requires: caml_js_from_array
function ui64_ui64_make_bytecode(unit) {
  return [0, [255, 123n], [255, 456n]];
}

//Provides: f64_f64_make
//Requires: caml_js_from_array
function f64_f64_make() {
  return [0, [254, 123], [254, 456]];
}

//Provides: f64_f64_make_bytecode
//Requires: caml_js_from_array
function f64_f64_make_bytecode(unit) {
  return [0, [254, 123], [254, 456]];
}