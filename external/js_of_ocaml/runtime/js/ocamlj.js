//Provides: caml_invalid_switch_arm
function caml_invalid_switch_arm() {
  throw "caml_invalid_switch_arm: encountered invalid switch arm, " +
        "this is a bug in Flambda2 or the Flambda2 -> JSIR pass";
}

//Provides: caml_invalid_primitive
function caml_invalid_primitive() {
  throw "caml_invalid_primitive: encountered an invalid primitive, " +
        "this is a bug in Flambda2 or the Flambda2 -> JSIR pass";
}

//Provides: caml_symbols
var caml_symbols = {};

//Provides: caml_register_symbol (const,const,mutable)
//Requires: caml_symbols
function caml_register_symbol(compilation_unit, symbol, value) {
  if (!caml_symbols[compilation_unit]) {
    caml_symbols[compilation_unit] = {};
  }
  caml_symbols[compilation_unit][symbol] = value;
}

//Provides: caml_get_symbol (const,const)
//Requires: caml_symbols
function caml_get_symbol(compilation_unit, symbol) {
  return caml_symbols[compilation_unit][symbol];
}
