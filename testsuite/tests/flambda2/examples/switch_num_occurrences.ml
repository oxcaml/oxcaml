(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-dlambda -dcanonical-ids -dcmm";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
 {
   flat-float-array;
   check-ocamlopt.byte-output;
 }
*)

(* We are testing two things on this small piece of code:

    a) The lambda output should contain two tests, not a full switch, because we
       expect to share branches in `lambda/matching.ml`.

    b) The cmm output should not contain an intermediate `let`, because all
       variables are used linearly.
 *)

type t =
  | A
  | B
  | C
  | D

let even_variant (t : t) : bool =
  match t with
  | A -> true
  | B -> false
  | C -> true
  | D -> false
