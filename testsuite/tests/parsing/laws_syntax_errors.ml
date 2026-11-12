(* TEST
 toplevel;
*)

(* [===>] is reserved for laws. *)

let ( ===> ) a b = not a || b;;

(* [===>] only separates the clauses of a law: it cannot appear inside an
   expression. *)

law_? nested (a : bool) (b : bool) : (a ===> b);;

(* Clauses are [expr]s, not sequences. *)

law_? seq (n : int) (a : bool) : ignore n; a ===> a;;

(* The name of a law is a lowercase identifier and cannot be omitted. *)

law_? : true;;

law_? Foo : true;;

(* The body cannot be omitted, nor the conclusion. *)

law_? foo true;;

law_? foo : a ===> ;;

(* Parameters are variables, optionally with a type annotation. *)

law_? foo (x, y) : true;;

law_? foo ~x : true;;

law_? foo _ : true;;

(* Without a space before the colon, [?name:] is an optional label. *)

law_?foo: true;;

law_ ?foo: true;;
