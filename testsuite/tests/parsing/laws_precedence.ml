(* TEST
 flags = "-stop-after parsing -dparsetree";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Each clause of a law is a complete [expr], so [implies_] binds less tightly
   than anything, and the body of a [let], [fun] or [match] case stops at it. *)

law_? infix (a : bool) (b : bool) : a && b implies_ a || b
law_? equality (x : int) (y : int) : x = y implies_ y = x
law_? if_ (a : bool) (b : bool) : if a then b implies_ b
law_? let_ (a : bool) : let b = a in b implies_ a
law_? fun_ (a : bool) : fun b -> b implies_ a
law_? match_ (a : bool) : match a with true -> a | false -> true implies_ a

(* Attributes on a clause attach to that clause, docstrings attach to the law,
   and a floating attribute after a law is an item of its own. *)

law_? clause_attrs : a [@attr_a] implies_ b [@attr_b]

(** Documented. *)
law_? documented : true
[@@@floating]
