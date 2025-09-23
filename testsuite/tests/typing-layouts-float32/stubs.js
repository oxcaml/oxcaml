//Provides: sin_U_U
//Requires: caml_sin_float32_bytecode
function sin_U_U(u1) {
    return caml_sin_float32_bytecode(u1);
}

//Provides: sin_B_U
//Requires: caml_sin_float32_bytecode
function sin_B_U(u1) {
    return caml_sin_float32_bytecode(u1);
}

//Provides: sin_U_B
//Requires: caml_sin_float32_bytecode
function sin_U_B(u1) {
    return caml_sin_float32_bytecode(u1);
}

//Provides: sin_byte
//Requires: caml_sin_float32_bytecode
function sin_byte(u1) {
    return caml_sin_float32_bytecode(u1);
}

//Provides: sum_7
function sum_7(x1, x2b, x3, x4b, x5, x6b, x7) {
  return (x1 + x2b + x3 + x4b + x5 + x6b + x7);
}

//Provides: sum_7_byte
function sum_7_byte(x1, x2b, x3, x4b, x5, x6b, x7) {
  return (x1 + x2b + x3 + x4b + x5 + x6b + x7);
}
