(* Reads the output of extern_standalone_test_big.c back with
   Marshal.from_channel and checks it against the expected list.

   Usage:  ocaml extern_standalone_check_big.ml actual_big.bin *)

let ic = open_in_bin Sys.argv.(1)

let () =
  let expected = List.init 10000 (fun i -> "item" ^ string_of_int i) in
  let v : string list = Marshal.from_channel ic in
  if v <> expected then (prerr_endline "big mismatch"; exit 1);
  print_endline "big readback OK"
