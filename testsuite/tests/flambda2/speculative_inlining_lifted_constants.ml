(* TEST
   compile_only = "true";
   flambda2;

   ocamlopt_flags += " -flambda2-inline-small-function-size 0";
   ocamlopt_flags += " -flambda2-inline-threshold 0";
   ocamlopt_flags += " -flambda2-speculative-inlining-track-lifted-constants";
   ocamlopt_flags += " -no-flambda2-speculative-inlining-only-if-arguments-useful";

   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-raw, dump-simplify;
   check-fexpr-dump;
 *)

[@@@ocaml.flambda_o3]

(* With the small function size and inlining threshold both at [0], we expect
   that none of the calls to [f] below should be inlined, because there is
   really nothing we can do except for avoiding the allocation of the pair,
   which should not be enough benefit.

   We used to have a bug where the size of lifted constants was not taken into
   account when doing speculative inlining, which would compute a code size of
   [1] (the cost of loading from the precomputed constant) for each of the
   calls to [f], causing them to be inlined (see oxcaml/oxcaml#5917 and
   commit 09f54a649f35fe5c572caf1485674672a14d75ff). *)

let f x = (x, x)

let zero = Sys.opaque_identity 0

let v0 = f zero

let one = Sys.opaque_identity 1

let v1 () = f one
