(* Consumer for the [included_alias] bundle.  In contrast to
   [main_pure_alias.ml], this compiles successfully: [Included_alias.Message]
   is bundle-local (not [Pruned_<head>]) because the
   [include Message] in the body recorded [Message] with [Exact] precision. *)

module Inst = Bundle_included_alias.Make (P_int) ()

let () = print_endline (Inst.Included_alias.Message.hello (P_int.create ()))
