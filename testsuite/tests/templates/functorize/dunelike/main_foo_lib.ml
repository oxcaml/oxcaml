(* Consumer for a dune-library-style bundle.  Only the wrapper [Foo] was
   passed to [-functorize] and it only re-exports [B].  [Foo__A] is not
   referenced by the wrapper at all; the functorizer still pulls it into
   the bundle transitively because [foo__B.cmi] lists it as [Exact].

   [Inst.DEP__Foo__.A] and [Inst.DEP__Foo__.B] are also accessible
   (transitively-pulled modules get a [DEP__] prefix in the bundle to
   discourage direct access).  [foo__.cmi] lists [Foo__A] and [Foo__B]
   as [Approximate] (it is pure aliases under -no-alias-deps), but they
   are [Exact] elsewhere ([foo__B.cmi] and [foo.cmi] respectively), so
   the functorizer loads them rather than pruning. *)

module Inst = Bundle_foo_lib.Make (P_int) ()

let () =
  let p = P_int.create () in
  print_endline (Inst.Foo.B.bye p);
  print_endline (Inst.DEP__Foo__.A.hello p);
  print_endline (Inst.DEP__Foo__.B.bye p)
