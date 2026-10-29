(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-flambda2-expert-shorten-symbol-names -zero-alloc-check opt";
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

module Nonallocating_primitive : sig
  val add : int8# -> int8# -> int8# [@@zero_alloc opt]
end = struct
  external add : int8# -> int8# -> int8# = "%int8#_add"
end

module Allocating_primitive : sig
  val add : int32 -> int32 -> int32 [@@zero_alloc opt] (* Should error *)
end = struct
  external add : int32 -> int32 -> int32 = "%int32_add" (* Should error *)
end
