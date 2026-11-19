(* TEST
 compile_only = "true";
 flambda2;
 only-default-codegen;
 flags = "-O3 -dcfg-invariants -stop-after selection";
 setup-ocamlopt.opt-build-env;
 ocamlopt.opt;
*)

(* Check the selected CFG before running register allocation.
   The unreachable B branch stores an int where the actual C block has a
   float. Mutable unboxing must not pass that int to the float continuation
   parameter used by the subsequent load. *)
external opaque_float : float# -> float# = "%opaque"

type t =
  | A of int
  | B of { x : int; mutable z : int }
  | C of { x : int; mutable y : float# }

let _ =
  let mutable m = C { x = 0; y = #1.0 } in
  for _ = 0 to 0 do () done;
  (match m with
  | B r -> r.z <- 42
  | A _ | C _ -> ());
  match m with
  | C r -> let _ = opaque_float r.y in ()
  | A _ | B _ -> ()
