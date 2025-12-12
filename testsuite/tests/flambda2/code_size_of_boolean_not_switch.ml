(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-flambda2-inline-small-function-size 1 -flambda2-inline-large-function-size 1 -dfexpr";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

let f b =
  (* The code size of f should be 2:
   *
   * let Pnot = %not b in  [%not: 1]
   * ret Pnot           [apply_cont: 1]
   *)
  (* Note: we use arithmetic obfuscation to prevent the frontend from being too
     smart *)
  if b
  then 9 - 9
  else 8 - 7

let g b =
  (* We should not inline with a small function size that is less than the code
     size of f. *)
  f b
