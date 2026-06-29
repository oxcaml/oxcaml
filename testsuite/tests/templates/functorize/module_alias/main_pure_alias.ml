(* Consumer for the [pure_alias] bundle.  [Pure_alias] declares
   [module Message = Message] under [-no-alias-deps], so [Message] is an
   [Approximate]-precision bound_global in its cmi.  The functorizer
   substitutes such references with [Pruned_<head>] (here [Pruned_Message]),
   so the alias chain [Inst.Pure_alias.Message] fails to compile here. *)

module Inst = Bundle_pure_alias.Make (P_int) ()

let () = print_endline (Inst.Pure_alias.Message.hello (P_int.create ()))
