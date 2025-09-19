//Provides: lognot_UtoU
function lognot_UtoU(u) {
  return ~u;
}

//Provides: lognot_BtoU
//Requires: caml_int64_of_bytes
function lognot_BtoU(b) {
  var u = caml_int64_of_bytes(b);
  return ~u;
}

//Provides: lognot_UtoB
//Requires: caml_int64_to_bytes
function lognot_UtoB(u) {
  return caml_int64_to_bytes(~u);
}

//Provides: lognot_bytecode
//Requires: caml_int64_of_bytes, caml_int64_to_bytes
function lognot_bytecode(b) {
  var u = caml_int64_of_bytes(b);
  return caml_int64_to_bytes(~u);
}

//Provides: sum_7_UBUBUBUtoU
//Requires: caml_int64_of_bytes
function sum_7_UBUBUBUtoU(u1, b2, u3, b4, u5, b6, u7) {
  var u2 = caml_int64_of_bytes(b2);
  var u4 = caml_int64_of_bytes(b4);
  var u6 = caml_int64_of_bytes(b6);
  return (u1 + u2 + u3 + u4 + u5 + u6 + u7);
}

//Provides: sum_7_bytecode
//Requires: caml_int64_of_bytes, caml_int64_to_bytes
function sum_7_bytecode(argv, argn) {
  if (argn != 7) return caml_int64_to_bytes(-1);
  var u1 = caml_int64_of_bytes(argv[0]);
  var u2 = caml_int64_of_bytes(argv[1]);
  var u3 = caml_int64_of_bytes(argv[2]);
  var u4 = caml_int64_of_bytes(argv[3]);
  var u5 = caml_int64_of_bytes(argv[4]);
  var u6 = caml_int64_of_bytes(argv[5]);
  var u7 = caml_int64_of_bytes(argv[6]);
  return caml_int64_to_bytes(u1 + u2 + u3 + u4 + u5 + u6 + u7);
}