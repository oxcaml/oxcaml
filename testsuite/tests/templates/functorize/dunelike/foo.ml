(* Parameters: P *)

(* [include] forces a body-level use of [B], so [Foo__B] lands in
   [foo.cmi]'s bound_globals with [Exact] precision even under
   -no-alias-deps.  [Foo__A] is not referenced by the wrapper at all —
   it is pulled into the bundle transitively via [foo__B.cmi]'s own
   [Exact] dep on [Foo__A].

   CR-soon zqian: this [include struct ... end] workaround shouldn't be
   necessary — we need a better way for the wrapper to make its submodule
   aliases carry [Exact] precision. *)
module B = struct include B end
