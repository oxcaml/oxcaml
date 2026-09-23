(* TEST
 readonly_files = "cells_observe_lib.ml cells_observe_stubs.c";
 flambda2;
 setup-ocamlopt.byte-build-env;
 module = "cells_observe_lib.ml";
 ocamlopt.byte;
 module = "cells_observe.ml";
 ocamlopt.byte;
 module = "";
 all_modules = "cells_observe_stubs.c cells_observe_lib.cmx cells_observe.cmx";
 program = "${test_build_directory}/cells_observe.exe";
 ocamlopt.byte;
 run;
 check-program-output;
*)

(* Observe the cells of [Cells_observe_lib] at runtime, through C stubs that
   name the cell symbols directly: each cell field (including the flat fields
   of unboxed values, whose cells put values before flats) must hold the same
   value as the field read through the normal access path. *)

external cell_size : int -> int = "cells_observe_cell_size"
external cell_word : int -> int -> int64 = "cells_observe_cell_word"
external cell_field : int -> int -> Obj.t = "cells_observe_cell_field"
external box_float : float# -> float = "%box_float"

let check name ok =
  Printf.printf "%s: %s\n" name (if ok then "OK" else "MISMATCH")

let () =
  List.iter
    (fun i -> Printf.printf "cell%d size = %d\n" i (cell_size i))
    [0; 1; 2; 3; 4];
  check "f 3 via cell0"
    ((Obj.obj (cell_field 0 0) : int -> int) 3 = Cells_observe_lib.f 3);
  check "n via cell1" ((Obj.obj (cell_field 1 0) : int) = Cells_observe_lib.n);
  check "u via cell2"
    (Int64.float_of_bits (cell_word 2 0) = box_float Cells_observe_lib.u);
  let #(p0, p1) = Cells_observe_lib.p in
  check "snd p via cell3" ((Obj.obj (cell_field 3 0) : int) = p1);
  check "fst p via cell3" (Int64.float_of_bits (cell_word 3 1) = box_float p0);
  let #(q0, q1) = Cells_observe_lib.q in
  check "snd q via cell4" ((Obj.obj (cell_field 4 0) : int) = q1);
  check "fst q via cell4" (Int64.float_of_bits (cell_word 4 1) = box_float q0)
