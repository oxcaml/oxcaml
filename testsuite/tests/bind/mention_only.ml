(* Compiled with [-no-alias-deps -bind Foo Lib__bar]: [Foo] is only
   mentioned, its components are not accessed. *)
module X = Foo
