(* TEST
 readonly_files = "cells_lib.ml";
 flambda2;
 {
   setup-ocamlopt.byte-build-env;
   module = "cells_lib.ml";
   ocamlopt.byte;
   ocamlopt_flags = "-dcmm -dcanonical-ids";
   module = "cells_use_cmm.ml";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }{
   ocamlopt_flags = "-Oclassic";
   compiler_directory_suffix = ".Oclassic";
   compiler_reference =
     "${test_source_directory}/cells_use_cmm.Oclassic.compilers.reference";
   setup-ocamlopt.byte-build-env;
   module = "cells_lib.ml";
   ocamlopt.byte;
   ocamlopt_flags = "-Oclassic -dcmm -dcanonical-ids";
   module = "cells_use_cmm.ml";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }
*)

(* Native code emits no module block for [Cells_lib], so its fields are read
   from its cells.  In the Cmm dump, in both Normal and Classic mode:
   - [Cells_lib.f], a static field, folds to the closure symbol;
   - [Cells_lib.n], [!Cells_lib.r] and [Cells_lib.u] are single loads from
     [camlCells_lib__cell1], [camlCells_lib__cell2] and [camlCells_lib__cell3]
     (no reconstruction of the whole block for a field access);
   - [(module Cells_lib : S)] rebuilds the block from the cells. *)

module type S = sig val n : int val r : int ref end

let f = Cells_lib.f
let n = Cells_lib.n
let get_r () = !Cells_lib.r
let u () = Cells_lib.u
let whole = (module Cells_lib : S)
