(* TEST
 reference =
   "${test_source_directory}/tuple_constructor_any_arg_inference.reference";
 flambda2;
 {
   flags = "-extension layouts_beta";
   native;
 }{
   flags = "-extension layouts_beta -Oclassic";
   native;
 }{
   flags = "-extension layouts_beta -O3";
   native;
 }{
   flags = "-extension layouts_beta";
   bytecode;
 }
*)

(* Specializing [any]-arguments in variants *)

external box_float : float# -> float = "%box_float"

type ('a : any) t = A of 'a

(* [x] later gets specialized to [float64] *)
let mk_t x = A x

let _ = mk_t #3.14

(* This function determines the representation via its annotations *)
let get_float (A x : _ t) : float# = x

type ('a : any) pair = P of 'a * int

(* In each of these, [x] later gets specialized to a different sort *)
let mk_up x = P (x, 1)
let mk_float x = P (x, 2)
let mk_string x = P (x, 3)

let () =
  let t = mk_t #2.5 in
  Printf.printf "get_float (mk_t #2.5): %.2f\n" (box_float (get_float t));
  let (P (#(i, s), n)) = mk_up #(21, "twenty-one") in
  Printf.printf "mk_up: %d %s %d\n" i s n;
  let (P (x, n)) = mk_float #6.25 in
  Printf.printf "mk_float #6.25: %.2f %d\n" (box_float x) n;
  let (P (s, n)) = mk_string "hi" in
  Printf.printf "mk_string: %s %d\n" s n
