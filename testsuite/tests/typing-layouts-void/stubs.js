//Provides: void_to_void
function void_to_void() {
}

//Provides: void_to_seven
function void_to_seven() {
  return 7;
}

//Provides: seven_to_void
function seven_to_void(seven) {
  if (seven !== 7) throw new Error("Assertion failed: seven !== 7");
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
