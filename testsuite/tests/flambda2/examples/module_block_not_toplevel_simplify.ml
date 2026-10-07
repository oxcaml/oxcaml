(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-raw, dump-simplify;
 check-fexpr-dump;
*)

[@@@ocaml.flambda_o3]
[@@@ocaml.warning "-8"]

(* The refutable pattern means that the module block is built inside the
   handler of a continuation that is not at the toplevel of the compilation
   unit (since the match may fail), so the module symbol cannot be bound
   directly to it: the block is passed to the continuation defining the module
   symbol instead. *)

let[@inline never] f x = x + 1

let y = f (Sys.opaque_identity 41)

let (Some z) = Sys.opaque_identity (Some y)
