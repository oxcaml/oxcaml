(* Consumer for the [uses_plain] bundle.  Verifies that the bundle's
   functor signature is [functor (P) () -> sig module Uses_plain : ... end]
   — no [Plain] in the bundle — and that the [Plain.t] reference in
   [Uses_plain]'s signature resolves correctly as a global at link time. *)

module Inst = Bundle_uses_plain.Make (P_int) ()

(* Call through Uses_plain.greet whose result type is Plain.t *)
let () = print_endline (Inst.Uses_plain.greet (P_int.create ()))

(* Access through the alias to Plain (not a parameterised module — should
   resolve to the global Plain) *)
let () = print_endline Inst.Uses_plain.Plain_alias.greeting
