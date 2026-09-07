(* TEST compile_only = "true"; flambda2; ocamlopt_flags = "-O3 -dcmm -dcanonical-ids";
   setup-ocamlopt.byte-build-env; ocamlopt.byte; check-ocamlopt.byte-output;
*)

(* to_cmm formalism validation: a Block_set of an immediate is a plain Cstore, whereas a
   Block_set of a heap pointer goes through the caml_modify GC write barrier; a dense int
   match compiles to a data-table load via array_indexing. Rules: TC.Prim.BlockSet,
   TC.Prim.ArrayAccess (ch. 18), CM.Store (ch. 15). Case study:
   middle_end/flambda2/docs/formalism/14-validation/tocmm-05-block-set-barrier.md *)

let[@inline never] setfst (r : int ref) v = r := v
let[@inline never] setref (r : string ref) s = r := s

let[@inline never] shape n =
  match n with
  | 0 -> "zero"
  | 1 -> "one"
  | 2 -> "two"
  | _ -> "many"
;;
