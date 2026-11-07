(* The transitively-pulled dep [Foo__] is [Unmentionable]: it has an
   identity in the bundle (the wrapper's [Foo] aliases into it) but a
   consumer cannot name it, not even via a dotted path. *)

module Inst = Bundle_foo_lib.Make (P_int) ()

let () = print_endline (Inst.Foo__.A.hello (P_int.create ()))
