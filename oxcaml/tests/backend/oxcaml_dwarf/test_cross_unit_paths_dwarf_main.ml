let () =
  Test_cross_unit_paths_dwarf.run ();
  let prim_time = Sys.opaque_identity Test_cross_unit_paths_dwarf.prim_time in
  ignore (Sys.opaque_identity (prim_time ()))
