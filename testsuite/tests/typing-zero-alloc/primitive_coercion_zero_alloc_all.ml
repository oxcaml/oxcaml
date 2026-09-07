(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-flambda2-expert-shorten-symbol-names";
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

[@@@zero_alloc all]

module Nonallocating_primitive : sig
  val add : int8# -> int8# -> int8#
end = struct
  external add : int8# -> int8# -> int8# = "%int8#_add"
end

module Allocating_primitive : sig
  val add : int32 -> int32 -> int32 (* Should error *)
end = struct
  external add : int32 -> int32 -> int32 = "%int32_add" (* Should error *)
end

module Zero_alloc_ignore : sig
  val add : int32 -> int32 -> int32 [@@zero_alloc ignore]
end = struct
  external add : int32 -> int32 -> int32 = "%int32_add"
end

module Uncoerced_primitive : sig
  external add : int32 -> int32 -> int32 = "%int32_add"
end = struct
  external add : int32 -> int32 -> int32 = "%int32_add"
end
