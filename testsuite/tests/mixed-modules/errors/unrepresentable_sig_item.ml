(* TEST
 include stdlib_upstream_compatible;
 flambda2;
 {
   setup-ocamlc.opt-build-env;
   ocamlc_opt_exit_status = "2";
   ocamlc.opt;
   check-ocamlc.opt-output;
 }{
   setup-ocamlopt.opt-build-env;
   ocamlopt_opt_exit_status = "2";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
 }
*)

module type S = sig
  type t : any
  val x : t
end
