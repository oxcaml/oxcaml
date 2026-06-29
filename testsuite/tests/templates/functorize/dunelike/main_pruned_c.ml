(* [Foo__C] is not referenced by the wrapper [Foo].  It appears only as
   an [Approximate] alias in [foo__.cmi] (never [Exact] anywhere), so
   the functorizer prunes it to [Pruned_Foo__C].  Accessing
   [Inst.DEP__Foo__.C] fails to compile. *)

module Inst = Bundle_foo_lib.Make (P_int) ()

let () = print_endline (Inst.DEP__Foo__.C.combined (P_int.create ()))
