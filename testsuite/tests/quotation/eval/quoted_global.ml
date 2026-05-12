(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  modules = "quoted_global_indirect_value.ml quoted_global_indirect_type.ml quoted_global_direct.ml";
  native;
*)

#syntax quotations on

let () =
  Printf.printf "direct: %d\n" (Eval.eval <[ Quoted_global_direct.x ]>)

let () =
  Printf.printf "indirect value: %d\n" (Eval.eval Quoted_global_direct.y)

let () =
  let _ = Eval.eval Quoted_global_direct.z in
  Printf.printf "indirect type\n"
