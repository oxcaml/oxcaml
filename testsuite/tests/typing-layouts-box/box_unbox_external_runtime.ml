(* TEST
   reference = "${test_source_directory}/box_unbox_external_runtime.reference";
   flags = "-extension small_numbers";
   include stdlib_upstream_compatible;
   include stdlib_stable;
   flambda2;
   {
     native;
   }{
     bytecode;
   }
*)

(* This file tests that %box and %unbox actually work at runtime,
   not just type-check. *)

external box : ('a : any). 'a -> 'a box_ = "%box"
[@@layout_poly]

external unbox : ('a : any). 'a box_ -> 'a = "%unbox"
[@@layout_poly]

module Float_u = Stdlib_upstream_compatible.Float_u
module Int32_u = Stdlib_upstream_compatible.Int32_u
module Int64_u = Stdlib_upstream_compatible.Int64_u
module Nativeint_u = Stdlib_upstream_compatible.Nativeint_u
module Float32_u = Stdlib_stable.Float32_u

(* Helpers *)
let print_test name passed =
  Printf.printf "%s: %s\n" name (if passed then "PASS" else "FAIL")

(*****************************************)
(* Test 1: float# roundtrip *)

let test_float () =
  let x = #3.14 in
  let boxed = box x in
  let unboxed = unbox boxed in
  let passed = Float_u.(equal x unboxed) in
  print_test "float# roundtrip" passed

(*****************************************)
(* Test 2: int32# roundtrip *)

let test_int32 () =
  let x = #42l in
  let boxed = box x in
  let unboxed = unbox boxed in
  let passed = Int32_u.(equal x unboxed) in
  print_test "int32# roundtrip" passed

(*****************************************)
(* Test 3: int64# roundtrip *)

let test_int64 () =
  let x = #123456789L in
  let boxed = box x in
  let unboxed = unbox boxed in
  let passed = Int64_u.(equal x unboxed) in
  print_test "int64# roundtrip" passed

(*****************************************)
(* Test 4: nativeint# roundtrip *)

let test_nativeint () =
  let x = #99n in
  let boxed = box x in
  let unboxed = unbox boxed in
  let passed = Nativeint_u.(equal x unboxed) in
  print_test "nativeint# roundtrip" passed

(*****************************************)
(* Test 5: float32# roundtrip *)

let test_float32 () =
  let x = #2.718s in
  let boxed = box x in
  let unboxed = unbox boxed in
  let passed = Float32_u.(equal x unboxed) in
  print_test "float32# roundtrip" passed

(*****************************************)
(* Test 6: value type roundtrip (should be identity) *)

let test_value () =
  let x = 42 in
  let boxed = box x in
  let unboxed = unbox boxed in
  let passed = (x = unboxed) in
  print_test "int (value) roundtrip" passed

(*****************************************)
(* Test 7: unbox from explicitly boxed type *)

let test_unbox_boxed () =
  let boxed : float = 3.14 in
  let unboxed : float# = unbox boxed in
  let reboxed : float = box unboxed in
  let passed = (boxed = reboxed) in
  print_test "unbox boxed float" passed

(*****************************************)
(* Test 8: nested box/unbox *)

let test_nested () =
  let x = #1.5 in
  (* float# -> float# box_ -> float# box_ box_ *)
  let double_boxed = box (box x) in
  (* float# box_ box_ -> float# box_ -> float# *)
  let double_unboxed = unbox (unbox double_boxed) in
  let passed = Float_u.(equal x double_unboxed) in
  print_test "nested box/unbox" passed

(*****************************************)
(* Test 9: void roundtrip *)

type void : void
external void : unit -> void = "%unbox_unit"

let test_void () =
  let v = void () in
  let boxed = box v in
  let _ : void = unbox boxed in
  (* If we got here without crashing, it works *)
  print_test "void roundtrip" true

(*****************************************)
(* Run all tests *)

let () =
  test_float ();
  test_int32 ();
  test_int64 ();
  test_nativeint ();
  test_float32 ();
  test_value ();
  test_unbox_boxed ();
  test_nested ();
  test_void ()
