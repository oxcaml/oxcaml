(* Generates the expected marshalled bytes for
   extern_standalone_test_big.c, using the real Marshal module.

   Usage:  ocaml extern_standalone_gen_expected_big.ml expected_big.bin *)

let out = open_out_bin Sys.argv.(1)

let () =
  let l = List.init 10000 (fun i -> "item" ^ string_of_int i) in
  output_string out (Marshal.to_string l []);
  close_out out
