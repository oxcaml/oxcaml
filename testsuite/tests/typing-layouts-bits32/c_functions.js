//Provides: lognot_UtoU
function lognot_UtoU(u) {
  return ~u | 0;
}

//Provides: lognot_BtoU
function lognot_BtoU(u) {
  return ~u | 0;
}

//Provides: lognot_UtoB
function lognot_UtoB(u) {
  return ~u | 0;
}

//Provides: lognot_bytecode
function lognot_bytecode(u) {
  return ~u | 0;
}

//Provides: sum_7_UBUBUBUtoU
function sum_7_UBUBUBUtoU(u1, b2, u3, b4, u5, b6, u7) {
  return (u1 + b2 + u3 + b4 + u5 + b6 + u7) | 0;
}

//Provides: sum_7_bytecode
//Requires: sum_7_UBUBUBUtoU
function sum_7_bytecode(u1, b2, u3, b4, u5, b6, u7) {
  return sum_7_UBUBUBUtoU(u1, b2, u3, b4, u5, b6, u7);
}
