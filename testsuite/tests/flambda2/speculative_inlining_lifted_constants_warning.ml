(* TEST
   compile_only = "true";
   flambda2;

   ocamlopt_flags += " -flambda2-inline-small-function-size 0";
   ocamlopt_flags += " -flambda2-inline-threshold 0";
   ocamlopt_flags += " -no-flambda2-speculative-inlining-only-if-arguments-useful";
   ocamlopt_flags += " -w +222";

   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 *)

[@@@ocaml.flambda_o3]

(* Same program as [speculative_inlining_lifted_constants.ml], but compiled
   without [-flambda2-speculative-inlining-track-lifted-constants].  Without
   that flag the size of the lifted constants is not taken into account by
   speculative inlining, so both calls to [f] are inlined, whereas with the
   flag they are not.  Warning 222 should therefore be reported at both call
   sites. *)

let f x = (x, x)

let zero = Sys.opaque_identity 0

let v0 = f zero

let one = Sys.opaque_identity 1

let v1 () = f one
