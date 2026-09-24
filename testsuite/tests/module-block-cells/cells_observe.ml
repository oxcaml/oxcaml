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
   name the cell symbols directly: each cell field must hold the same word as
   the module block field it mirrors (including the flat fields of unboxed
   values, whose cells put values before flats just as the block does), and
   the same value as the field read through the normal access path. *)

external block_size : unit -> int = "cells_observe_block_size"
external cell_size : int -> int = "cells_observe_cell_size"
external block_word : int -> int64 = "cells_observe_block_word"
external cell_word : int -> int -> int64 = "cells_observe_cell_word"
external cell_field : int -> int -> Obj.t = "cells_observe_cell_field"
external box_float : float# -> float = "%box_float"

(* [cell, cell field, block field, name].  The physical layout of the block is
   values first, then flats: [f; n; snd p; snd q; u; fst p; fst q]. *)
let layout =
  [ 0, 0, 0, "f";
    1, 0, 1, "n";
    2, 0, 4, "u";
    3, 0, 2, "snd p";
    3, 1, 5, "fst p";
    4, 0, 3, "snd q";
    4, 1, 6, "fst q" ]

let check name ok =
  Printf.printf "%s: %s\n" name (if ok then "OK" else "MISMATCH")

let () =
  Printf.printf "block size = %d\n" (block_size ());
  List.iter
    (fun i -> Printf.printf "cell%d size = %d\n" i (cell_size i))
    [0; 1; 2; 3; 4];
  List.iter
    (fun (cell, cell_pos, block_pos, name) ->
      check
        (Printf.sprintf "cell%d[%d] = block[%d] (%s)" cell cell_pos block_pos
           name)
        (Int64.equal (cell_word cell cell_pos) (block_word block_pos)))
    layout;
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
