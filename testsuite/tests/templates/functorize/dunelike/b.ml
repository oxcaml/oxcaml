(* Parameters: P *)

(* Depends on [A] concretely (which resolves through the [Foo__] alias to
   [Foo__A]).  This makes [Foo__A] an [Exact] dep of [Foo__B.cmi]. *)

let bye (p : P.t) = A.hello p ^ " B.bye"
