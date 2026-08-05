(* TEST compile_only = "true"; flambda2; ocamlopt_flags = "-O3 -dcmm -dcanonical-ids";
   setup-ocamlopt.byte-build-env; ocamlopt.byte; check-ocamlopt.byte-output;
*)

(* to_cmm formalism validation: integer tagging, block build/load, and the static-closure
   layout. Rules: R.Val.Imm, R.Header, R.Obj.Block, R.Obj.Closures, TC.Prim.MakeBlock,
   TC.Prim.BlockLoad (ch. 17, 18). Case study:
   middle_end/flambda2/docs/formalism/14-validation/tocmm-01-tagging-blocks.md Phenomena:
   [f] is [(+ x 2)]; [pair] is [(alloc 2048 a b)] (header 2 lsl 10); [g] loads fields at
   offsets 0 and 8; closure headers/closinfo encode size/tag/arity. *)

let[@inline never] pair a b = a, b
let f x = x + 1
let g p = fst p + snd p
