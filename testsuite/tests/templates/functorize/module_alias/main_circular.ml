(* Consumer for the [circular_alias] bundle.  Both [Mod_a] and [Mod_b]
   alias each other; access the value [foo] through a long alias chain
   that hops between them. *)

module Inst = Bundle_circular.Make (P_int) ()

let () =
  let p = P_int.create () in
  print_endline
    (Inst.Mod_a.Mod_b_alias.Mod_a_alias.Mod_b_alias.Mod_a_alias.foo p)
