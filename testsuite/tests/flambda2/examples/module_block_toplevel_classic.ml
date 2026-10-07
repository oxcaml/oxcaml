(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-raw;
 check-fexpr-dump;
*)

[@@@ocaml.flambda_oclassic]

(* The module block is built at the toplevel of the compilation unit (the
   handler of the continuation for the tuple pattern counts as toplevel), so
   the module symbol is bound directly to it. *)

let[@inline never] f x = x + 1

let y = f (Sys.opaque_identity 41)

let a, b = Sys.opaque_identity (y, "s")
