(* TEST
 reference = "${test_source_directory}/optimized.reference";
 flambda2;
 {
   native;
 } {
   flags = "-O3";
   native;
 } {
   flags = "-Oclassic";
   native;
 } {
   bytecode;
 }
*)

type ('a : value_or_null) atomic = { mutable contents : 'a [@atomic] }

let print_string_atomic prefix x = Printf.printf "%s: %s\n" prefix x.contents
let print_int_atomic prefix x = Printf.printf "%s: %d\n" prefix x.contents
let print_float_atomic prefix x = Printf.printf "%s: %.2f\n" prefix x.contents
let print_int_or_null_atomic prefix x = match x.contents with
  | Null -> Printf.printf "%s: Null\n" prefix
  | This n -> Printf.printf "%s: This %d\n" prefix n

let print_float_or_null_atomic prefix x = match x.contents with
  | Null -> Printf.printf "%s: Null\n" prefix
  | This n -> Printf.printf "%s: This %.2f\n" prefix n

(* Test getting and setting int atomic fields *)
let test_int_atomic () =
  let x = { contents = 42 } in
  print_int_atomic "Int atomic get initial" x;
  x.contents <- 100;
  print_int_atomic "Int atomic get after set" x;
;;

let () = test_int_atomic ()

(* Test getting and setting string atomic fields *)
let test_string_atomic () =
  let x = { contents = "hello" } in
  print_string_atomic "String atomic get initial" x;
  x.contents <- "world";
  print_string_atomic "String atomic get after set" x;
;;

let () = test_string_atomic ()

(* Test getting and setting float atomic fields *)
let test_float_atomic () =
  let x = { contents = 3.14 } in
  print_float_atomic "Float atomic get initial" x;
  x.contents <- 2.71;
  print_float_atomic "Float atomic get after set" x;
;;

let () = test_float_atomic ()

(* Test getting and setting or_null atomic fields *)
let test_or_null_atomic () =
  let x = { contents = This 123 } in
  print_int_or_null_atomic "Or_null atomic get initial" x;
  x.contents <- Null;
  print_int_or_null_atomic "Or_null atomic get after set to Null" x;
  x.contents <- This 456;
  print_int_or_null_atomic "Or_null atomic get after set to This" x;

  let y = { contents = This 1.23 } in
  print_float_or_null_atomic "Float or_null atomic get initial" y;
  y.contents <- Null;
  print_float_or_null_atomic "Float or_null atomic get after set to Null" y;
  y.contents <- This 4.56;
  print_float_or_null_atomic "Float or_null atomic get after set to This" y;
;;

let () = test_or_null_atomic ()
