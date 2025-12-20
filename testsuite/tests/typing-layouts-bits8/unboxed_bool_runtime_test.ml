(* TEST
 modules = "bool_stubs.c";
 reference = "${test_source_directory}/unboxed_bool_runtime_test.reference";
 native;
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
 }
*)

let test_num = ref 0
let start_test name =
  incr test_num;
  Printf.printf "Test %d: %s\n" !test_num name

let ocaml_not = function
  | #false -> #true
  | #true -> #false

let ocaml_box = function
  | #false -> false
  | #true -> true

let ocaml_unbox = function
  | false -> #false
  | true -> #true

external c_not : bool# -> bool# = "not_bytecode" "not" 
external c_box : bool# -> bool = "box_bytecode" "box" 
external c_unbox : bool -> bool# = "unbox_bytecode" "unbox" 

(****************************)
(* Test 1: expressions *)

let test1 () =
  start_test "expressions";

  assert (ocaml_box #true);
  assert (c_box #true);

  assert (not (ocaml_box #false));
  assert (not (c_box #false));

  assert (ocaml_box (ocaml_not #false));
  assert (ocaml_box (c_not #false));
  assert (c_box (ocaml_not #false));
  assert (c_box (c_not #false));

  assert (ocaml_box (ocaml_unbox true));
  assert (ocaml_box (c_unbox true));
  assert (c_box (ocaml_unbox true));
  assert (c_box (c_unbox true));

  assert (ocaml_box (ocaml_not (ocaml_unbox false)));
  assert (ocaml_box (ocaml_not (c_unbox false)));
  assert (ocaml_box (c_not (ocaml_unbox false)));
  assert (ocaml_box (c_not (c_unbox false)));
  assert (c_box (ocaml_not (ocaml_unbox false)));
  assert (c_box (ocaml_not (c_unbox false)));
  assert (c_box (c_not (ocaml_unbox false)));
  assert (c_box (c_not (c_unbox false)));

  (* The same C functions, eta-expanded *)
  let c_not = c_not in
  let c_box = c_box in
  let c_unbox = c_unbox in
  assert (c_box #true);
  assert (not (c_box #false));
  assert (ocaml_box (c_not #false));
  assert (c_box (ocaml_not #false));
  assert (c_box (c_not #false));
  assert (ocaml_box (c_unbox true));
  assert (c_box (ocaml_unbox true));
  assert (c_box (c_unbox true));
  assert (ocaml_box (ocaml_not (c_unbox false)));
  assert (ocaml_box (c_not (ocaml_unbox false)));
  assert (ocaml_box (c_not (c_unbox false)));
  assert (c_box (ocaml_not (ocaml_unbox false)));
  assert (c_box (ocaml_not (c_unbox false)));
  assert (c_box (c_not (ocaml_unbox false)));
  assert (c_box (c_not (c_unbox false)));

  ()

let _ = test1 ()

(****************************************)
(* Test 2: matching *)

let f1 = function
  | #false -> 0
  | #true -> 1

let f2 = function
  | #(#false, #false) -> 0
  | #(#false, #true) -> 1
  | #(#true, #false) -> 2
  | #(#true, #true) -> 3

let f3 = function
  | #(#false, #false, #false) -> 0
  | #(#false, #false, #true) -> 1
  | #(#false, #true, #false) -> 2
  | #(#false, #true, #true) -> 3
  | #(#true, #false, #false) -> 4
  | #(#true, #false, #true) -> 5
  | #(#true, #true, #false) -> 6
  | #(#true, #true, #true) -> 7

let test2 () =
  start_test "matching";

  assert (f1 #false = 0);
  assert (f1 #true = 1);

  assert (f2 #(#false, #false) = 0);
  assert (f2 #(#false, #true) = 1);
  assert (f2 #(#true, #false) = 2);
  assert (f2 #(#true, #true) = 3);

  assert (f3 #(#false, #false, #false) = 0);
  assert (f3 #(#false, #false, #true) = 1);
  assert (f3 #(#false, #true, #false) = 2);
  assert (f3 #(#false, #true, #true) = 3);
  assert (f3 #(#true, #false, #false) = 4);
  assert (f3 #(#true, #false, #true) = 5);
  assert (f3 #(#true, #true, #false) = 6);
  assert (f3 #(#true, #true, #true) = 7);

  ()

let _ = test2 ()

