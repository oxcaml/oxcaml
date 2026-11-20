(* TEST
 compile_only = "true";
 flambda2;
 only-default-codegen;
 flags = "-O3 -dcfg-invariants -stop-after selection";
 setup-ocamlopt.opt-build-env;
 ocamlopt.opt;
*)

(* Check the selected CFG before running register allocation.
   The unreachable B branch loads a value at field index 1, where the actual
   C block has a float. Mutable unboxing must not substitute that float for z. *)
external opaque : unit -> 'a = "%opaque"
type t =
  | A of int
  | B of int * int8# * int
  | C of { x : int; mutable y : float# }
let _ =
  try
    ignore
      (ignore
         (match
            let mutable m = C { y = #1.0; x = 0 } in
            for _ = 0 to 0 do () done; m
          with
          | B (_, _, z) -> (ignore (string_of_int z); opaque ())
          | A z -> (ignore ((opaque ()) z); opaque ()));
       ());
    opaque ()
  with Exit -> ()
