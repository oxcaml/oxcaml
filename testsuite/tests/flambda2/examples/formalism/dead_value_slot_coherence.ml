(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Formalism validation: a variable captured only in a value slot that is never
   projected in the whole unit is dropped; the closure references the lifted
   symbol directly and carries no value slot.
   Rules: INV.Simplify.DeadValueSlotCoherence (ch. 13), S.Rewrite.Prim.Projection
   (ch. 10); to_cmm face INV.ToCmm.SlotLiveness (ch. 20).
   Case study: middle_end/flambda2/docs/formalism/14-validation/dead_value_slot_coherence.md
   Phenomenon: the closure body folds the projection to the lifted pair symbol,
   so the slot leaves used_value_slots and is dropped, even though the closure
   escapes via [opaque]. *)

external opaque : 'a -> 'a = "%opaque"

let[@inline never] mk () =
  let x = (1, 2) in
  fun () -> x

let f = mk ()
let r = (opaque f) ()
