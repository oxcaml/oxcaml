(* TEST
 compile_only = "true";
 ocamlopt_flags = "-flambda2-inline-small-function-size 0 -flambda2-inline-threshold 7.99 -dfexpr-after simplify";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

external string_length : string -> int = "%string_length"

let f x =
  (* Code size of f is 8:
   *
   * let prim = %string_length x in       [%string_length: 5]
   * let Pstringlength = %Tag_imm prim in [%Tag_imm: 2]
   * ret (Pstringlength)                  [apply_cont: 1]
   *)
  string_length x

let g x =
  (* Inlining does not actually improve the code, so we should choose
     *not* to inline with a threshold just below the size of [f] (8). *)
  f x
