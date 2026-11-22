(* TEST
 readonly_files = "parallel_slices.ml";
 setup-ocamlc.byte-build-env;
 module = "parallel_slices.ml";
 ocamlc.byte;
 module = "parallel_slice_capture.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

open Parallel_slices

let replace targets =
  Slice.fork_join2 (Slice.of_array targets) ~pivot:1
    (fun _left -> Target_array.set targets 1 Broadcast)
    (fun _right -> ()) [@nontail]
