(* TEST
 compile_only = "true";
 ocamlopt_flags = "-O3";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* When a record field is known to be a value array, Is_flat_float_array
   should be simplified to false and removed by the simplifier. *)

type foo = A | B of string

type t = { a : int; b : int; c : foo array }

let sum (t : t) =
  Array.fold_left (fun acc x ->
    match x with A -> acc | B _ -> acc + 1) 0 t.c
