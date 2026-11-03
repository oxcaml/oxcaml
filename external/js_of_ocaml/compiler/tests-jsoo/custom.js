//Provides: process
var process = "process";

//Provides: obj
var obj = { process: 42 };

//Provides: jsoo_test_boxed_nativeint_to_int64
//Requires: caml_int64_of_nativeint
function jsoo_test_boxed_nativeint_to_int64(x) {
  return caml_int64_of_nativeint(x);
}
