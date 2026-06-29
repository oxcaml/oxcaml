(* Parameters: P *)

(* Uses both [A] and [B] concretely (through the [Foo__] alias).
   [Foo__C.cmi] lists [Foo__A] and [Foo__B] as [Exact] deps. *)

let combined (p : P.t) = A.hello p ^ " " ^ B.bye p
