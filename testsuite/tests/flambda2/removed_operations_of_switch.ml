(* TEST
 compile_only = "true";
 ocamlopt_flags = "-flambda2-inline-small-function-size 26 -flambda2-inline-threshold 26.99 -flambda2-inline-large-function-size 100 -dfexpr";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)


let f b x y =
  (* Code size of f is 27:
   *
   * let untagged = %untag_imm b in [%untag_imm: 1]
   * switch untagged with           [switch: 0]
   * | 0 -> k0                      [switch arm: 5]
   * | 1 -> k1                      [switch arm: 5]
   * where k0 =
   *   let Pmakeblock = %Block 0 (x, y) in
   *                                [block(n): 5 + n = 7]
   *   ret Pmakeblock               [apply_cont: 1]
   * where k1 =
   *   let Pmakeblock = %Block 0 (y, x) in
   *                                [block(n): 5 + n = 7]
   *   ret Pmakeblock               [apply_cont: 1]
   *)
  if b
  then (x, y)
  else (y, x)

let g b x y =
  (* Inlining does not actually improve the code, so we should
     choose *not* to inline with a threshold just below the size
     of [f]. *)
  f b x y
