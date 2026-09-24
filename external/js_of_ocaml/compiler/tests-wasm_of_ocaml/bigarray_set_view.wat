(module
   (import "env" "caml_ba_get_data"
      (func $get_data (param (ref eq)) (result (ref extern))))
   (import "env" "caml_ba_get_view"
      (func $get_view (param (ref eq)) (result (ref extern))))
   (import "env" "caml_ba_set_data"
      (func $set_data (param (ref eq)) (param (ref extern))))
   (import "env" "caml_ba_set_view"
      (func $set_view (param (ref eq)) (param (ref extern))))
   (import "env" "caml_ba_uint8_get16"
      (func $get16 (param (ref eq)) (param i32) (result i32)))

   (func (export "test_bigarray_replace_backing")
      (param $dst (ref eq)) (param $src (ref eq)) (result (ref eq))
      (call $set_data (local.get $dst) (call $get_data (local.get $src)))
      (call $set_view (local.get $dst) (call $get_view (local.get $src)))
      (ref.i31 (i32.const 0)))

   (func (export "test_bigarray_get16")
      (param $ba (ref eq)) (param $index (ref eq)) (result (ref eq))
      (ref.i31
         (call $get16 (local.get $ba)
            (i31.get_s (ref.cast (ref i31) (local.get $index)))))))
