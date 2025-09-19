//Provides: sin_U_U
function sin_U_U(u1) {
  return Math.sin(u1);
}

//Provides: sin_B_U
//Requires: caml_float32_val
function sin_B_U(u1) {
  return Math.sin(caml_float32_val(u1));
}

//Provides: sin_U_B
//Requires: caml_copy_float32
function sin_U_B(u1) {
  return caml_copy_float32(Math.sin(u1));
}

//Provides: sin_byte
//Requires: caml_float32_val, caml_copy_float32, sin_U_U
function sin_byte(u1) {
  return caml_copy_float32(sin_U_U(caml_float32_val(u1)));
}

//Provides: sum_7
//Requires: caml_float32_val
function sum_7(x1, x2b, x3, x4b, x5, x6b, x7) {
  var x2 = caml_float32_val(x2b);
  var x4 = caml_float32_val(x4b);
  var x6 = caml_float32_val(x6b);
  return (x1 + x2 + x3 + x4 + x5 + x6 + x7);
}

//Provides: sum_7_byte
//Requires: caml_float32_val, caml_copy_float32
function sum_7_byte(argv, argn) {
  var x1 = caml_float32_val(argv[0]);
  var x2 = caml_float32_val(argv[1]);
  var x3 = caml_float32_val(argv[2]);
  var x4 = caml_float32_val(argv[3]);
  var x5 = caml_float32_val(argv[4]);
  var x6 = caml_float32_val(argv[5]);
  var x7 = caml_float32_val(argv[6]);
  return caml_copy_float32(x1 + x2 + x3 + x4 + x5 + x6 + x7);
}