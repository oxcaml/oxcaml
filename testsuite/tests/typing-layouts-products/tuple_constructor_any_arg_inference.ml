(* TEST
 flags = "-extension layouts_beta";
 flambda2;
 compile_only = "true";
 ocamlopt_opt_exit_status = "2";
 setup-ocamlopt.opt-build-env;
 ocamlopt.opt;
*)

(* A variable-representation variant used in functions that don't constrain the
   sort variable *)

external box_float : float# -> float = "%box_float"

type ('a : any) t = A of 'a

let f x =
  let t = A x in
  t

let _ = f #3.14

let g (A x : _ t) : float# = x

type ('a : any) pair = P of 'a * int

let mk x =
  let p = P (x, 1) in
  p

let mk_up () = mk #(21, "twenty-one")

let mk_float x =
  let p = P (x, 2) in
  p

let () =
  let t = f #2.5 in
  Printf.printf "g (f #2.5): %.2f\n" (box_float (g t));
  let (P (#(i, s), n)) = mk_up () in
  Printf.printf "mk_up: %d %s %d\n" i s n;
  let (P (x, n)) = mk_float #6.25 in
  Printf.printf "mk_float #6.25: %.2f %d\n" (box_float x) n
