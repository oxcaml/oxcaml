//Provides: partition_test_b_only
function partition_test_b_only() { return 101; }

//Provides: partition_test_c_only
function partition_test_c_only() { return 113; }

//Provides: partition_test_record
//Requires: caml_jsstring_of_string
function partition_test_record(value) {
  globalThis.partitionTest.events.push(caml_jsstring_of_string(value));
  return 0;
}

//Provides: partition_test_expose_main
//Requires: caml_callback
function partition_test_expose_main(callback) {
  globalThis.partitionTest.main = function () { return caml_callback(callback, [0]); };
  return 0;
}

//Provides: partition_test_expose_lazy
//Requires: caml_callback
function partition_test_expose_lazy(callback) {
  globalThis.partitionTest.lazy = function () { return caml_callback(callback, [0]); };
  return 0;
}

//Provides: partition_test_expose
//Requires: caml_jsstring_of_string, caml_callback
function partition_test_expose(name, callback) {
  globalThis.partitionTest[caml_jsstring_of_string(name)] = function (...args) {
    return caml_callback(callback, args.length === 0 ? [0] : args);
  };
  return 0;
}

//Provides: partition_test_expose_js
function partition_test_expose_js(name, callback) {
  globalThis.partitionTest[name] = function (...args) {
    return callback(...(args.length === 0 ? [0] : args));
  };
}

//Provides: partition_test_record_js
function partition_test_record_js(value) {
  globalThis.partitionTest.events.push(value);
}

//Provides: partition_test_expose_main_js
function partition_test_expose_main_js(callback) {
  globalThis.partitionTest.main = function () { return callback(0); };
}

//Provides: partition_test_expose_lazy_js
function partition_test_expose_lazy_js(callback) {
  globalThis.partitionTest.lazy = function () { return callback(0); };
}
