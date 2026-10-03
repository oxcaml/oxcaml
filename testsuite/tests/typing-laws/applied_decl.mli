(* TEST
 flags = "-extension laws";
 setup-ocamlopt.opt-build-env;
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

(* The check of an interface against itself reports a law that refers to a
   value through a functor application. *)

module F (X : sig end) : sig val x : int ref end
module A : sig end
module M := F (A)
law? p : M.x == M.x
