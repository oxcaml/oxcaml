//Provides: sin_byte
//Requires: caml_sin_float32_bytecode
function sin_byte(u1) {
  return caml_sin_float32_bytecode(u1)
}

//Provides: sum_7_byte
//Requires: caml_add_float32
function sum_7_byte(x1, x2b, x3, x4b, x5, x6b, x7) {
  var result = x1;
  result = caml_add_float32(result, x2b);
  result = caml_add_float32(result, x3);
  result = caml_add_float32(result, x4b);
  result = caml_add_float32(result, x5);
  result = caml_add_float32(result, x6b);
  result = caml_add_float32(result, x7);
  return result;
}
