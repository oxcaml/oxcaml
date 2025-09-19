//Provides: sin_U_U
function sin_U_U(u1) {
  return Math.sin(u1);
}

//Provides: sin_B_U
//Requires: caml_floatarray_load_f64
function sin_B_U(u1) {
  return Math.sin(caml_floatarray_load_f64(u1, 0));
}

//Provides: sin_U_B
//Requires: caml_float_return
function sin_U_B(u1) {
  return caml_float_return(Math.sin(u1));
}

//Provides: sin_byte
//Requires: caml_floatarray_load_f64, caml_float_return
function sin_byte(u1) {
  return caml_float_return(Math.sin(caml_floatarray_load_f64(u1, 0)));
}

//Provides: sum_7
//Requires: caml_floatarray_load_f64
function sum_7(x1, x2b, x3, x4b, x5, x6b, x7) {
  var x2 = caml_floatarray_load_f64(x2b, 0);
  var x4 = caml_floatarray_load_f64(x4b, 0);
  var x6 = caml_floatarray_load_f64(x6b, 0);
  return (x1 + x2 + x3 + x4 + x5 + x6 + x7);
}

//Provides: sum_7_byte
//Requires: caml_floatarray_load_f64, caml_float_return
function sum_7_byte(argv) {
  var x1 = caml_floatarray_load_f64(argv[1], 0);
  var x2 = caml_floatarray_load_f64(argv[2], 0);
  var x3 = caml_floatarray_load_f64(argv[3], 0);
  var x4 = caml_floatarray_load_f64(argv[4], 0);
  var x5 = caml_floatarray_load_f64(argv[5], 0);
  var x6 = caml_floatarray_load_f64(argv[6], 0);
  var x7 = caml_floatarray_load_f64(argv[7], 0);
  return caml_float_return(x1 + x2 + x3 + x4 + x5 + x6 + x7);
}