(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-flambda2-inline-small-function-size 0 -flambda2-inline-large-function-size 0 -dfexpr";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* Need at least 3 cases in a match to be considered a
   "single_arg_to_same_destination" switch *)
type t = A | B | C

let switch_converted_to_lookup_table x =
  (* Make sure these are not affinely related! *)
  match x with
  | A -> 1
  | B -> 2
  | C -> 4

let switch_converted_to_lookup_table' x =
  (* We should never inline with a small function size of 0! *)
  switch_converted_to_lookup_table x

let switch_converted_to_affine x =
  match x with
  | A -> 7
  | B -> 8
  | C -> 9

let switch_converted_to_affine' x =
  (* We should never inline with a small function size of 0! *)
  switch_converted_to_affine x
