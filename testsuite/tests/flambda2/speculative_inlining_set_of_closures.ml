(* TEST
   compile_only = "true";
   flambda2;

   ocamlopt_flags += " -flambda2-inline-small-function-size 0";
   ocamlopt_flags += " -flambda2-inline-threshold 60";
   ocamlopt_flags += " -flambda2-speculative-inlining-track-lifted-constants";
   ocamlopt_flags += " -no-flambda2-speculative-inlining-only-if-arguments-useful";

   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-simplify;
   check-fexpr-dump;
 *)

[@@@ocaml.flambda_o3]

(* We used to have a bug where the code size of a set of closures was counted
   twice by speculative inlining when the set could not be lifted: once as part
   of the cost of the set of closures itself, and once via the (lifted) code
   bindings, which were tracked as lifted constants.  Sets of closures that
   could be lifted were only counted once, as the cost metrics of lifted sets
   of closures did not include the size of the code.

   Both calls to [g] below should be inlined: the
   cost of inlining either of them is around 50 (mostly the code size of [f],
   which is 36), which is below the threshold of 60.  With the
   double-counting bug, the speculative cost of inlining [g] in [r2] was
   around 86, so the call in [r2] was not inlined, unlike the one in [r1]. *)

let[@inline never] use f = f () + f ()

let g x =
  let f () =
    let y = Sys.opaque_identity x in
    (y * 2) + (y * 4) + (y * 6) + (y * 8) + (y * 10) + (y * 12)
  in
  use f

let r1 () = g 1

let r2 () = g (Sys.opaque_identity 1)