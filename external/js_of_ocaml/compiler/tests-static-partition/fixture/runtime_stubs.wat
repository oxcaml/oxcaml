(module
  (import "env" "caml_jsstring_of_string"
    (func $jsstring (param (ref eq)) (result (ref eq))))
  (import "env" "caml_js_wrap_callback"
    (func $callback (param (ref eq)) (result (ref eq))))
  (import "env" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
  (import "js" "partition_test_record_js" (func $record (param anyref)))
  (import "js" "partition_test_expose_js" (func $expose (param anyref anyref)))
  (import "js" "partition_test_expose_main_js" (func $main (param anyref)))
  (import "js" "partition_test_expose_lazy_js" (func $lazy (param anyref)))

  (func (export "partition_test_b_only") (param (ref eq)) (result (ref eq))
    (ref.i31 (i32.const 101)))
  (func (export "partition_test_c_only") (param (ref eq)) (result (ref eq))
    (ref.i31 (i32.const 113)))
  (func (export "partition_test_expose") (param $name (ref eq)) (param $value (ref eq)) (result (ref eq))
    (call $expose
      (call $unwrap (call $jsstring (local.get $name)))
      (call $unwrap (call $callback (local.get $value))))
    (ref.i31 (i32.const 0)))
  (func (export "partition_test_record") (param $value (ref eq)) (result (ref eq))
    (call $record (call $unwrap (call $jsstring (local.get $value))))
    (ref.i31 (i32.const 0)))
  (func (export "partition_test_expose_main") (param $value (ref eq)) (result (ref eq))
    (call $main (call $unwrap (call $callback (local.get $value))))
    (ref.i31 (i32.const 0)))
  (func (export "partition_test_expose_lazy") (param $value (ref eq)) (result (ref eq))
    (call $lazy (call $unwrap (call $callback (local.get $value))))
    (ref.i31 (i32.const 0))))
