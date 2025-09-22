// JavaScript stub implementation for bigarrcml test
// Provides JavaScript equivalents of the C functions

//Provides: c_filltab
//Requires: caml_ba_create, caml_js_from_array
function c_filltab() {
  var DIMX = 6;
  var DIMY = 8;

  // Create dims as an OCaml array
  var dims = [0, DIMX, DIMY]; // OCaml arrays start with tag 0

  // Create a Float64 Bigarray with C layout
  var ba = caml_ba_create(
    1,  // Float64
    0,  // C_layout
    dims
  );

  // Fill the array with values: x * 100 + y
  for (var x = 0; x < DIMX; x++) {
    for (var y = 0; y < DIMY; y++) {
      ba.data[x * DIMY + y] = x * 100 + y;
    }
  }

  return ba;
}

//Provides: c_printtab
function c_printtab(ba) {
  var DIMX = 6;
  var DIMY = 8;

  // Print the array in the same format as the C version
  for (var x = 0; x < DIMX; x++) {
    var line = "  " + x;
    for (var y = 0; y < DIMY; y++) {
      var val = ba.data[x * DIMY + y];
      // Format number with 1 decimal place, right-padded to 8 chars
      var formatted = val.toFixed(1);
      while (formatted.length < 8) {
        formatted = " " + formatted;
      }
      line += formatted;
    }
    console.log(line);
  }

  return 0; // Return unit
}