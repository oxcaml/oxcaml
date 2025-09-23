(* TEST
 modules = "stubs.c stubs.js";
 reference = "${test_source_directory}/void_stubs_runtime_test.reference";
 flambda2;
 {
   native;
 }{
   flags = "-Oclassic";
   native;
 }{
   flags = "-O3";
   native;
 }{
   bytecode;
 }{
   javascript;
 }
*)

(***********)
(* Prelude *)

type void : void mod everything
external void : unit -> void = "%unbox_unit"

type r = #{ v1 : void; v2 : void }

let test_num = ref 0
let start_test name =
  incr test_num;
  Printf.printf "Test %d: %s\n" !test_num name

let[@inline never] use_void (v : void) =
  let _ : void = v in
  1

(****************************)
(* Test 1: void arg/returns *)

external void_to_void : void -> void = "void_to_void_bytecode" "void_to_void"
external void_to_seven : void -> int = "void_to_seven_bytecode" "void_to_seven"
external seven_to_void : int -> void = "seven_to_void_bytecode" "seven_to_void"

let test1 () =
  start_test "void args/returns";
  assert (use_void (void_to_void (void ())) = 1);
  assert (void_to_seven (void ()) = 7);
  assert (use_void (seven_to_void 7) = 1);
  (* the same test, eta-expanded *)
  let void_to_void = void_to_void in
  let void_to_seven = void_to_seven in
  let seven_to_void = seven_to_void in
  assert (use_void (void_to_void (void ())) = 1);
  assert (void_to_seven (void ()) = 7);
  assert (use_void (seven_to_void 7) = 1);
  ()

let _ = test1 ()

(****************************************)
(* Test 2: void arg mixed with int args *)

external six_to_void_to_seven : int -> void -> int = "six_to_void_to_seven_bytecode" "six_to_seven"
external void_to_six_to_seven : void -> int -> int = "void_to_six_to_seven_bytecode" "six_to_seven"
external void_to_six_to_void_to_seven : void -> int -> void -> int = "void_to_six_to_void_to_seven_bytecode" "six_to_seven"
external six_to_void_to_seven_to_eight : int -> void -> int -> int = "six_to_void_to_seven_to_eight_bytecode" "six_to_seven_to_eight"

let test2 () =
  start_test "void args mixed with int args";
  assert (six_to_void_to_seven 6 (void ()) = 7);
  assert (void_to_six_to_seven (void ()) 6 = 7);
  assert (void_to_six_to_void_to_seven (void ()) 6 (void ()) = 7);
  assert (six_to_void_to_seven_to_eight 6 (void ()) 7 = 8);
  (* the same test, eta-expanded *)
  let six_to_void_to_seven = six_to_void_to_seven in
  let void_to_six_to_seven = void_to_six_to_seven in
  let void_to_six_to_void_to_seven = void_to_six_to_void_to_seven in
  let six_to_void_to_seven_to_eight = six_to_void_to_seven_to_eight in
  assert (six_to_void_to_seven 6 (void ()) = 7);
  assert (void_to_six_to_seven (void ()) 6 = 7);
  assert (void_to_six_to_void_to_seven (void ()) 6 (void ()) = 7);
  assert (six_to_void_to_seven_to_eight 6 (void ()) 7 = 8);
  ()

let _ = test2 ()

(************************************)
(* Test 3: products of void returns *)

external void_to_void_void : void -> #(void * void) = "void_to_void_void_bytecode" "void_to_void"
external void_to_void_seven : void -> #(void * int) = "void_to_void_seven_bytecode" "void_to_seven"
external void_to_seven_void : void -> #(int * void) = "void_to_seven_void_bytecode" "void_to_seven"

let test3 () =
  start_test "products of void returns";
  let #(v1, v2) = void_to_void_void (void ()) in
  assert (use_void v1 = 1 && use_void v2 = 1);
  let #(v, i) = void_to_void_seven (void ()) in
  assert (use_void v = 1 && i = 7);
  let #(i, v) = void_to_seven_void (void ()) in
  assert (use_void v = 1 && i = 7);
  (* the same test, eta-expanded *)
  let void_to_void_void = void_to_void_void in
  let void_to_void_seven = void_to_void_seven in
  let void_to_seven_void = void_to_seven_void in
  let #(v1, v2) = void_to_void_void (void ()) in
  assert (use_void v1 = 1 && use_void v2 = 1);
  let #(v, i) = void_to_void_seven (void ()) in
  assert (use_void v = 1 && i = 7);
  let #(i, v) = void_to_seven_void (void ()) in
  assert (use_void v = 1 && i = 7);
  ()

let _ = test3 ()

(****************************)

let () = print_endline "All tests passed."
