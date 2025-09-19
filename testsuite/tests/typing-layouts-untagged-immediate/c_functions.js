//Provides: lognot_UtoU
function lognot_UtoU(u) {
  return ~u;
}

//Provides: lognot_BtoU
function lognot_BtoU(v) {
  return ~v;
}

//Provides: lognot_UtoB
function lognot_UtoB(u) {
  return ~u;
}

//Provides: lognot_bytecode
function lognot_bytecode(v) {
  return ~v;
}

//Provides: sum_7_UBUBUBUtoU
function sum_7_UBUBUBUtoU(u1, b2, u3, b4, u5, b6, u7) {
  return u1 + b2 + u3 + b4 + u5 + b6 + u7;
}

//Provides: sum_7_bytecode
function sum_7_bytecode(argv, argn) {
  var sum = 0;
  for (var i = 0; i < 7; i++) {
    sum += argv[i];
  }
  return sum;
}

//Provides: is_sign_extended
function is_sign_extended(u) {
  if (u !== ((u << 1) >> 1)) {
    return 1;
  } else {
    return 0;
  }
}

//Provides: invalid_sign_bit
function invalid_sign_bit(unit) {
  return (-1 >>> 1);
}

//Provides: negative_one
function negative_one(unit) {
  return -1;
}

//Provides: return_true
function return_true(ignored) {
  return 1; // Val_true is 1 in JavaScript backend
}