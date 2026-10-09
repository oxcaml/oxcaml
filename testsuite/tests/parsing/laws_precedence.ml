(* TEST
 flags = "-stop-after parsing -dparsetree";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* [===>] is not an operator: each clause of a law is a complete [expr], so
   [===>] binds less tightly than anything, and the body of a [let], [fun]
   or [match] case stops at it. *)

law? infix (a : bool) (b : bool) : a && b ===> a || b
law? equality (x : int) (y : int) : x = y ===> y = x
law? if_ (a : bool) (b : bool) : if a then b ===> b
law? let_ (a : bool) : let b = a in b ===> a
law? fun_ (a : bool) : fun b -> b ===> a
law? match_ (a : bool) : match a with true -> a | false -> true ===> a
