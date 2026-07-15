(* TEST
 flambda2;
 ocamlopt_flags = "-support-lto";
 compile_only = "true";
 setup-ocamlopt.opt-build-env;
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

(* This test checks that the [-support-lto] flag causes the placeholder
   "LTO enabled" message to be printed during compilation. *)

(* CR mvellacott: This tests placeholder functionality and will need to be removed when
   the features is implemented and the placeholder removed. *)

let () = ()
