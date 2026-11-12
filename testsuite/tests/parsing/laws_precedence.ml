(* TEST
 flags = "-stop-after parsing -dparsetree";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* [===>] is not an operator: each clause of a law is a complete [expr], so
   [===>] binds less tightly than anything, and the body of a [let], [fun]
   or [match] case stops at it. *)

law_? infix (a : bool) (b : bool) : a && b ===> a || b
law_? equality (x : int) (y : int) : x = y ===> y = x
law_? if_ (a : bool) (b : bool) : if a then b ===> b
law_? let_ (a : bool) : let b = a in b ===> a
law_? fun_ (a : bool) : fun b -> b ===> a
law_? match_ (a : bool) : match a with true -> a | false -> true ===> a

(* Attributes on a clause attach to that clause, docstrings attach to the law,
   and a floating attribute after a law is an item of its own. *)

law_? clause_attrs : a [@attr_a] ===> b [@attr_b]

(** Documented. *)
law_? documented : true
[@@@floating]
