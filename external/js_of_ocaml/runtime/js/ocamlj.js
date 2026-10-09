// Runtime support for code produced by OxCaml's ocamlj compiler, which
// translates Flambda 2 terms to the js_of_ocaml IR instead of going through
// bytecode.

//Provides: caml_invalid_switch_arm
//If: oxcaml
function caml_invalid_switch_arm() {
  throw (
    "caml_invalid_switch_arm: encountered invalid switch arm, " +
    "this is a bug in Flambda2 or the Flambda2 -> JSIR pass"
  );
}

//Provides: caml_invalid_primitive
//If: oxcaml
function caml_invalid_primitive() {
  throw (
    "caml_invalid_primitive: encountered an invalid primitive, " +
    "this is a bug in Flambda2 or the Flambda2 -> JSIR pass"
  );
}

//Provides: caml_invalid_expr
//If: oxcaml
function caml_invalid_expr(msg) {
  throw "caml_invalid_expr: reached an Invalid Flambda2 expression: " + msg;
}

// Global symbol table, indexed by compilation unit then by symbol name.
// Flambda 2 symbols that are exported from a compilation unit are registered
// here when the unit is initialised, and looked up by the units that refer to
// them.

//Provides: caml_symbols
//If: oxcaml
var caml_symbols = {};

//Provides: caml_register_symbol (const,const,mutable)
//Requires: caml_symbols
//If: oxcaml
function caml_register_symbol(compilation_unit, symbol, value) {
  if (!caml_symbols[compilation_unit]) {
    caml_symbols[compilation_unit] = {};
  }
  caml_symbols[compilation_unit][symbol] = value;
}

//Provides: caml_get_symbol (const,const)
//Requires: caml_symbols
//If: oxcaml
function caml_get_symbol(compilation_unit, symbol) {
  return caml_symbols[compilation_unit][symbol];
}
