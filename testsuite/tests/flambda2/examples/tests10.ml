(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-rectypes";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-raw, dump-simplify;
 check-fexpr-dump;
*)

(* needs -rectypes *)

let rec f () = f, f
