(* Reads the output of extern_standalone_test.c back with
   Marshal.from_channel and checks each value, proving the standalone
   output is consumable by the real OCaml runtime.

   Usage:  ocaml extern_standalone_check_readback.ml actual.bin *)

let ic = open_in_bin Sys.argv.(1)

type t = A of int | B of int | C of int | D of string

let idx = ref 0

let check : 'a. 'a -> unit = fun expected ->
  incr idx;
  let v = Marshal.from_channel ic in
  if v <> expected then begin
    Printf.eprintf "value %d mismatch\n" !idx;
    exit 1
  end

let () =
  check 42;
  let pair = (1, 2) in
  check pair;
  check "hello";
  check (pair, pair);
  check 3.14;
  check [| 1.5; 2.5; 3.5 |];
  check [1; 2; 3];
  check "";
  check (-5);
  check (-1000);
  check 100000;
  check (1 lsl 40);
  check (String.make 100 'x');
  check (String.make 300 'y');
  check (D "payload");
  check ([||] : int array);
  check ([1; 2; 3], "mid", 2.25);
  (match input_char ic with
   | exception End_of_file -> ()
   | _ -> prerr_endline "trailing data"; exit 1);
  print_endline "readback OK"
