(* Innermost unit of the cross-unit inline chain of
   [test_cross_unit_paths_dwarf]. Compiled from within [cross_unit_dir/] with a
   matching [-directory] argument; see gen/gen_dune.ml. *)

let[@inline always] inner x =
  let y = Sys.opaque_identity (x * 3) in
  y + 1
