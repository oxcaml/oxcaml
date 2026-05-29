(* Consumer for the [pure_alias] bundle.  Exercises the original dune-style
   bug: accessing a value through an alias chain that goes through a sibling
   parameterised module in the bundle.  Without path compression, the alias
   [Pure_alias.Message] would resolve to the global [Message] (which isn't
   accessible here because we don't take [-parameter P]); with path
   compression, the alias points to the bundle-local [Message]. *)

module Inst = Bundle_pure_alias.Func (P_int) ()

(* Access via the alias chain (the original failing case): *)
let () = print_endline (Inst.Pure_alias.Message.hello (P_int.create ()))
(* Direct access (always worked): *)
let () = print_endline (Inst.Message.hello (P_int.create ()))
