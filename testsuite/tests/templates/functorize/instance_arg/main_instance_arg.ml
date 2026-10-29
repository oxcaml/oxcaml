(* [User]'s prelude pure-aliases the complete instance
   [Lib_q[Q:Q_impl]] under [-no-alias-deps], so [user__.cmi] records it
   approximately with the arg value over-approximated as [Q_impl{P}].
   The functorizer must complete the value against q_impl.cmi (which has
   no parameters), recognise the instance as static, and leave it as a
   global reference rather than bundling or locally instantiating it. *)

module Inst = Bundle.Make (P_int) ()

let () = print_endline (Inst.User.describe (P_int.create ()))
