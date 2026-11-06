;; This ensures that the referenced JavaScript values are linked in the
;; runtime with separate compilation and is optimized away in case of
;; whole program compilation.

(global (export "_caml_js_delete") (import "js" "caml_js_delete") anyref)
(global (export "_process") (import "js" "process") anyref)
(global (export "_obj") (import "js" "obj") anyref)

;; Exercises the runtime's boxed_nativeint_to_i64 export.
(import "env" "boxed_nativeint_to_i64"
   (func $boxed_nativeint_to_i64 (param (ref eq)) (result i64)))
(import "env" "caml_copy_int64"
   (func $caml_copy_int64 (param i64) (result (ref eq))))

(func (export "jsoo_test_boxed_nativeint_to_int64")
   (param $v (ref eq)) (result (ref eq))
   (call $caml_copy_int64 (call $boxed_nativeint_to_i64 (local.get $v))))
