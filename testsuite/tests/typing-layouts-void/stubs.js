//Provides: void_to_void
function void_to_void() {
  return 0;
}

//Provides: void_to_seven
function void_to_seven() {
  return 7;
}

//Provides: seven_to_void
function seven_to_void(seven) {
  if (seven !== 7) throw new Error("Assertion failed: seven !== 7");
  return 0;
}

//Provides: void_to_void_bytecode
function void_to_void_bytecode(unit) {
  if (unit !== 0) throw new Error("Assertion failed: unit !== 0");
  return 0;
}

//Provides: void_to_seven_bytecode
function void_to_seven_bytecode(unit) {
  if (unit !== 0) throw new Error("Assertion failed: unit !== 0");
  return 7;
}

//Provides: seven_to_void_bytecode
function seven_to_void_bytecode(seven) {
  if (seven !== 7) throw new Error("Assertion failed: seven !== 7");
  return 0;
}

//Provides: six_to_seven
function six_to_seven(six) {
  if (six !== 6) throw new Error("Assertion failed: six !== 6");
  return 7;
}

//Provides: six_to_seven_to_eight
function six_to_seven_to_eight(six, seven) {
  if (six !== 6) throw new Error("Assertion failed: six !== 6");
  if (seven !== 7) throw new Error("Assertion failed: seven !== 7");
  return 8;
}

//Provides: void_to_six_to_void_to_seven_bytecode
function void_to_six_to_void_to_seven_bytecode(unit1, six, unit2) {
  if (unit1 !== 0) throw new Error("Assertion failed: unit1 !== 0");
  if (unit2 !== 0) throw new Error("Assertion failed: unit2 !== 0");
  if (six !== 6) throw new Error("Assertion failed: six !== 6");
  return 7;
}

//Provides: six_to_void_to_seven_to_eight_bytecode
function six_to_void_to_seven_to_eight_bytecode(six, unit, seven) {
  if (unit !== 0) throw new Error("Assertion failed: unit !== 0");
  if (six !== 6) throw new Error("Assertion failed: six !== 6");
  if (seven !== 7) throw new Error("Assertion failed: seven !== 7");
  return 8;
}

//Provides: six_to_void_to_seven_bytecode
function six_to_void_to_seven_bytecode(six, unit) {
  if (unit !== 0) throw new Error("Assertion failed: unit !== 0");
  if (six !== 6) throw new Error("Assertion failed: six !== 6");
  return 7;
}

//Provides: void_to_six_to_seven_bytecode
function void_to_six_to_seven_bytecode(unit, six) {
  if (unit !== 0) throw new Error("Assertion failed: unit !== 0");
  if (six !== 6) throw new Error("Assertion failed: six !== 6");
  return 7;
}

//Provides: void_to_void_void_to_seven_bytecode
function void_to_void_void_to_seven_bytecode(unit1, unit2) {
  if (unit1 !== 0) throw new Error("Assertion failed: unit1 !== 0");
  if (unit2 !== 0) throw new Error("Assertion failed: unit2 !== 0");
  return 7;
}

//Provides: void_to_seven_void_bytecode
function void_to_seven_void_bytecode(unit) {
  return [0, 7, 0];
}

//Provides: void_to_void_seven_bytecode
function void_to_void_seven_bytecode(unit) {
  return [0, 0, 7];
}

//Provides: void_to_void_void_bytecode
function void_to_void_void_bytecode(unit) {
  return [0, 0, 0];
}