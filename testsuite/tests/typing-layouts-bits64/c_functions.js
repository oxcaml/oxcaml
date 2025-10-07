//Provides: lognot_UtoU
//Requires: MlInt64
function lognot_UtoU(u) {
  return u.xor(MlInt64.UNSIGNED_MAX);
}

//Provides: lognot_BtoU
//Requires: MlInt64
function lognot_BtoU(b) {
  return b.xor(MlInt64.UNSIGNED_MAX);
}

//Provides: lognot_UtoB
//Requires: MlInt64
function lognot_UtoB(u) {
  return u.xor(MlInt64.UNSIGNED_MAX);
}

//Provides: lognot_bytecode
//Requires: MlInt64
function lognot_bytecode(b) {
  return b.xor(MlInt64.UNSIGNED_MAX);
}

//Provides: sum_7_UBUBUBUtoU
function sum_7_UBUBUBUtoU(u1, b2, u3, b4, u5, b6, u7) {
  return u1.add(b2).add(u3).add(b4).add(u5).add(b6).add(u7);
}

//Provides: sum_7_bytecode
//Requires: sum_7_UBUBUBUtoU
function sum_7_bytecode(u1, b2, u3, b4, u5, b6, u7) {
  return sum_7_UBUBUBUtoU(u1, b2, u3, b4, u5, b6, u7);
}
