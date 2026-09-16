(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-flambda2-expert-shorten-symbol-names";
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

external add8 : int8# -> int8# -> int8# = "%int8#_add"

external add32 : int32 -> int32 -> int32 = "%int32_add" (* Should error *)

external add64 : int64 -> int64 -> int64 = "%int64_add"
