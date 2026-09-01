(* Middle unit of the cross-unit inline chain of [test_cross_unit_paths_dwarf].
   Compiled from within [cross_unit_dir/] with a matching [-directory] argument;
   see gen/gen_dune.ml. *)

let[@inline always] outer x =
  let y = Cu_lib_inner.inner (x + 2) in
  y * Sys.opaque_identity 5
