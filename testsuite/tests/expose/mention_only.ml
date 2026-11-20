(* Compiled with [-no-alias-deps -expose Lib__bar Foo]: [Foo] is only
   mentioned, its components are not accessed. *)
module X = Foo
