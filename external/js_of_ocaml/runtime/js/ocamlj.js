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

//Provides: caml_invalid_expr (const)
function caml_invalid_expr(msg) {
  throw "caml_invalid_expr: encountered an invalid expression " +
        "with the message: \n" + msg +
        "\n This is a bug in Flambda2 or the Flambda2 -> JSIR pass";
}

//Provides: caml_register_symbol (const,mutable)
function caml_register_symbol(symbol, value) {
  globalThis.jsoo_runtime[symbol] = value;
}

//Provides: caml_get_symbol (const)
function caml_get_symbol(symbol) {
  return globalThis.jsoo_runtime[symbol];
}
