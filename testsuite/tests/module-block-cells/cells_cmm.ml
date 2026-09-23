(* TEST
 compile_only = "true";
 flambda2;
 {
   ocamlopt_flags = "-dcmm -dcanonical-ids";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }{
   ocamlopt_flags = "-Oclassic -dcmm -dcanonical-ids";
   compiler_directory_suffix = ".Oclassic";
   compiler_reference =
     "${test_source_directory}/cells_cmm.Oclassic.compilers.reference";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }
*)

(* Each module field gets its own statically allocated cell,
   [camlCells_cmm__cell<i>], next to the module block.  In the Cmm dump:
   - cell0 (the closure of [f]) is prefilled with the closure symbol;
   - cell1 ([n], computed at initialisation) holds a placeholder that the
     entry function stores into, and is therefore listed in gc_roots;
   - cell2 (the unboxed [u]) is a one-field mixed block, prefilled.
   Of the cells, only cell1 is a gc root (as is the module block). *)

let f x = x + 1
let n = Sys.opaque_identity 3
let u = #4.0
