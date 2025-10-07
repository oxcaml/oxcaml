
//Provides: ui64_ui64_make
//Requires: caml_int64_of_int32
function ui64_ui64_make(unit) {
  return [0, caml_int64_of_int32(123), caml_int64_of_int32(456)];
}

//Provides: f64_f64_make
function f64_f64_make(unit) {
  return [0, 123, 456];
}
