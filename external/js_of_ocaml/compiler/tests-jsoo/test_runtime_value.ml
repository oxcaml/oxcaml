open Js_of_ocaml

let%expect_test "Jsoo_runtime.Js.runtime_value" =
  let p : Js.js_string Js.t = Jsoo_runtime.Js.runtime_value "process" in
  let o : _ Js.t = Jsoo_runtime.Js.runtime_value "obj" in
  let del = Jsoo_runtime.Js.runtime_value "caml_js_delete" in
  ignore (Js.Unsafe.fun_call del [| o; Js.Unsafe.coerce (Js.string "process") |]);
  print_endline (Js.to_string p);
  [%expect {| process |}]

external boxed_nativeint_to_int64 : nativeint -> int64
  = "jsoo_test_boxed_nativeint_to_int64"

let%expect_test "boxed_nativeint_to_i64" =
  List.iter
    (fun n ->
      let i = boxed_nativeint_to_int64 n in
      assert (Int64.equal i (Int64.of_nativeint n));
      print_endline (Int64.to_string i))
    [ -1n; Nativeint.min_int; Nativeint.max_int ];
  [%expect {|
           -1
           -2147483648
           2147483647
           |}]
