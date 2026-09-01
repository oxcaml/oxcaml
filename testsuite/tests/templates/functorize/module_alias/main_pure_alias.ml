(* Consumer for the [pure_alias] bundle.  [Pure_alias] declares only
   [module Message = Message] under [-no-alias-deps]; the functorizer
   loads [Message]'s cmi anyway and bundles it, so the alias chain
   [Inst.Pure_alias.Message] works. *)

module Inst = Bundle_pure_alias.Make (P_int) ()

let () = print_endline (Inst.Pure_alias.Message.hello (P_int.create ()))
