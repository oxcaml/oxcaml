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
 }{
   ocamlopt_flags = "-opaque -dcmm -dcanonical-ids";
   compiler_directory_suffix = ".opaque";
   compiler_reference =
     "${test_source_directory}/cells_cmm.opaque.compilers.reference";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }
*)

(* Each module field gets its own statically allocated cell,
   [camlCells_cmm__cell<i>]; native code emits no module block
   ([camlCells_cmm]), and the entry function returns a dummy.  In the Cmm
   dump:
   - cell0 (the closure of [f]) is prefilled with the closure symbol;
   - cell1 ([n], computed at initialisation) holds a placeholder that the
     entry function stores into, and is therefore listed in gc_roots;
   - cell2 (the unboxed [u]) is a one-field mixed block, prefilled;
   - cell3 and cell4 (the unboxed products [p] and [q]) are mixed blocks
     holding the int before the float, whatever their order in the product;
   - the cells stay [global] under -opaque, when no .cmx is produced. *)

let f x = x + 1
let n = Sys.opaque_identity 3
let u = #4.0
let p = #(#5.0, 6)
let q = #(#7.0, Sys.opaque_identity 8)
