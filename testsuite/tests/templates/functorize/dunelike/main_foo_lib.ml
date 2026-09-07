(* Consumer for a dune-library-style bundle.  Only the wrapper [Foo] was
   passed to [-functorize] and it only re-exports [B].  The functorizer
   still pulls [Foo__A], [Foo__B] and [Foo__C] into the bundle: every
   module referenced from a loaded cmi's bound_globals is loaded and
   bundled, even ones only recorded as pure aliases under
   -no-alias-deps (e.g. [Foo__C], referenced only by [foo__.cmi]).

   [Inst.DEP__Foo__.A]/[.B]/[.C] are accessible (transitively-pulled
   modules get a [DEP__] prefix in the bundle to discourage direct
   access). *)

module Inst = Bundle_foo_lib.Make (P_int) ()

let () =
  let p = P_int.create () in
  print_endline (Inst.Foo.B.bye p);
  print_endline (Inst.DEP__Foo__.A.hello p);
  print_endline (Inst.DEP__Foo__.B.bye p);
  print_endline (Inst.DEP__Foo__.C.combined p)
