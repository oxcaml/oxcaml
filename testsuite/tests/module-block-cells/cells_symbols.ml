(* TEST
 readonly_files = "cells_lib.ml check_cells.sh";
 flambda2;
 setup-ocamlopt.byte-build-env;
 module = "cells_lib.ml";
 ocamlopt.byte;
 module = "cells_symbols.ml";
 ocamlopt.byte;
 module = "";
 all_modules = "cells_lib.cmx cells_symbols.cmx";
 program = "${test_build_directory}/cells_symbols.exe";
 ocamlopt.byte;
 run;
 check-program-output;
 script = "sh ${test_source_directory}/check_cells.sh";
 script;
*)

(* The symbol tables of a compiled cells unit and of a program linking it:
   cells defined, no module block symbol (see check_cells.sh). *)

let () = Printf.printf "n = %d\n" Cells_lib.n
