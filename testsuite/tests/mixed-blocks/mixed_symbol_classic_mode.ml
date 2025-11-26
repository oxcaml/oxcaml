(* TEST
  modules = "mixed_symbol_classic_mode_dep.ml";
*)

[@@@ocaml.flambda_oclassic]

let () =
  match Mixed_symbol_classic_mode_dep.a.y with
  | Some 1 -> ()
  | Some _ | None -> assert false
