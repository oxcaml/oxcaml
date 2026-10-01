(* TEST
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* [===>] only separates the clauses of a law: it cannot appear inside an
   expression. *)

law? nested (a : bool) (b : bool) : (a ===> b)
