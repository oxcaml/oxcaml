(* TEST
 flambda2;
 flags += "-Oclassic -flambda2-reaper -X reaper-oclassic=1 -reaper-local-fields -reaper-debug-flags=nostamps";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-raw, dump-reaper;
 check-fexpr-dump;
*)

[@@@ocaml.flambda_oclassic]

(* The specialisation site declares only the functions that are used: [_g]
   is never called, so it has no declaration, rather than a deleted one, which
   the fexpr printer cannot print. *)

let[@inline never] outer y n =
  let rec f x = if x = 0 then y else f (x - 1)
  and _g z = f z + _g (z - 1) in
  f n

let _ = outer 5 3
