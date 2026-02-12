(* TEST
   modules = "symbol_projections_mixed_blocks_dep.ml";
   compile_only = "true";
   flambda2;
   setup-ocamlopt.opt-build-env;
   ocamlopt.opt;
   check-ocamlopt.opt-output;
*)

[@@@ocaml.flambda_o3]

module D = Symbol_projections_mixed_blocks_dep

let f_int64 (z : int) =
  let v = D.the_unboxed_int64 in
  let g () = v in
  z, g

let f_int32 (z : int) =
  let v = D.the_unboxed_int32 in
  let g () = v in
  z, g

let f_nativeint (z : int) =
  let v = D.the_unboxed_nativeint in
  let g () = v in
  z, g

let f_float (z : int) =
  let v = D.the_unboxed_float in
  let g () = v in
  z, g

let f_float32 (z : int) =
  let v = D.the_unboxed_float32 in
  let g () = v in
  z, g
