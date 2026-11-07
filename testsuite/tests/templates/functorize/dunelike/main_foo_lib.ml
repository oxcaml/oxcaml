(* Consumer for a dune-library-style bundle.  Only the wrapper [Foo] was
   passed to [-functorize]; the transitively-pulled [Foo__*] modules are
   bundled too, but as [Unmentionable] (see [../unmentionable]), so only
   [Foo] can be named through the instance. *)

module Inst = Bundle_foo_lib.Make (P_int) ()

let () = print_endline (Inst.Foo.B.bye (P_int.create ()))
