(* TEST
 readonly_files = "cells_lib.ml";
 flambda2;
 setup-ocamlopt.byte-build-env;
 module = "cells_lib.ml";
 ocamlopt.byte;
 module = "cells_run.ml";
 ocamlopt.byte;
 module = "";
 all_modules = "cells_lib.cmx cells_run.cmx";
 program = "${test_build_directory}/cells_run.exe";
 ocamlopt.byte;
 run;
 check-program-output;
*)

(* Behaviour of a module whose fields are also emitted as cells is
   unchanged, whether the fields are static, dynamic, mutable or unboxed. *)

external box_float : float# -> float = "%box_float"

let () =
  Printf.printf "f 3 = %d\n" (Cells_lib.f 3);
  Printf.printf "n = %d\n" Cells_lib.n;
  Printf.printf "r = %d\n" !Cells_lib.r;
  Cells_lib.r := 7;
  Printf.printf "r = %d\n" !Cells_lib.r;
  Printf.printf "u = %.1f\n" (box_float Cells_lib.u)
