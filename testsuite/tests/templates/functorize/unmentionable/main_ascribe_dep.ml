(* The transitively-pulled dep [Foo__A] is [Unmentionable] in the bundle
   instance.  Ascribing a signature that exports it is rejected by
   [Includemod]: the implementation's item is unmentionable but the
   interface requires it to be exported. *)

module Inst : sig
  module Foo__A : sig
    val hello : P_int.t -> string
  end
end =
  Bundle_foo_lib.Make (P_int) ()
