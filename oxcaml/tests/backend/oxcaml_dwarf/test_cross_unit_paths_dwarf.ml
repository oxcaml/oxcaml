(* [Cu_prim] was compiled from a foreign source with this file's basename. The
   interface's ordinary [val] forces a whole-unit primitive wrapper. *)
include Cu_prim

let[@inline never] [@local never] f_start () = ()

let _ = f_start ()

(* Checks the DWARF file paths recorded for code inlined from *other compilation
   units* that were compiled in *different working directories*, as happens in
   build systems that compile each directory in place (passing [-directory] to
   identify the source directory). The two modules in [cross_unit_dir/] are
   compiled from within that subdirectory (see the custom build rule in
   gen/gen_dune.ml) and [@inline always] ensures the chain [f_caller] ->
   [Cu_lib_outer.outer] -> [Cu_lib_inner.inner] is fully inlined into this unit.

   The backtrace from a breakpoint inside [inner]'s inlined body must show
   [inner] at [cross_unit_dir/cu_lib_inner.ml] (its line-table position) and
   [outer] at [cross_unit_dir/cu_lib_outer.ml] (the call site of [inner]). A
   regression that emits the foreign files' bare names, leaving them to be
   resolved against *this* unit's directory, shows both at nonexistent
   [oxcaml_dwarf/...] paths instead. *)

let[@inline never] [@local never] f_caller x =
  let result = Cu_lib_outer.outer (Sys.opaque_identity x) in
  Sys.opaque_identity result

let run () =
  let result = f_caller 11 in
  print_int result;
  print_newline ()
