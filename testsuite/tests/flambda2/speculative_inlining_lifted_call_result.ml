(* TEST
 flambda2;
 ocamlopt_flags += " -O3 -no-flambda2-reaper -dflambda-invariants";
 ocamlopt_flags += " -flambda2-inline-small-function-size 0";
 ocamlopt_flags += " -flambda2-inline-threshold 0";
 ocamlopt_flags += " -no-flambda2-speculative-inlining-only-if-arguments-useful";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
 run;
 check-program-output;
*)

[@@@ocaml.flambda_o3]

let[@inline never] produce n = Sys.opaque_identity n

(* During speculative inlining at unit toplevel, [captured] is a continuation
   parameter used only by the lifted closure. Its dependency must be recorded
   even though whole-unit code reachability is unavailable. *)
let make n =
  let captured = produce n in
  fun () -> captured

let get = make 42

let () = assert (get () = 42)
