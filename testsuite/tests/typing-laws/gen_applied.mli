(* TEST
 flags = "-extension laws";
 setup-ocamlopt.opt-build-env;
 module = "gen_applied.mli";
 ocamlopt.opt;
 flags = "-generate-laws-implementation -o gen_applied_laws.ml";
 module = "gen_applied.cmi";
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

(* The law of [N] refers to a value through a functor application. Nothing
   compares it, so the interface compiles; generating its laws reports
   it. *)

module F (X : sig end) : sig val x : int ref end
module G (X : sig end) : sig end
module A : sig end
module Behind (X : sig val x : int ref end) : sig
  module type T = sig law? p : X.x == X.x end
end
module N : Behind (F (G (A))).T
