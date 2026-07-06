(* Generates the expected marshalled bytes for extern_standalone_test.c,
   using the real Marshal module.  Values must match, in order, those in
   extern_standalone_test.c.

   Usage:  ocaml extern_standalone_gen_expected.ml expected.bin
   then compare against the test output with
   extern_standalone_compare.py. *)

let out = open_out_bin Sys.argv.(1)

let dump v = output_string out (Marshal.to_string v [])

(* [D _] is the fourth non-constant constructor, so it has tag 3 *)
type t = A of int | B of int | C of int | D of string

let () =
  dump 42;
  let pair = (1, 2) in
  dump pair;
  dump "hello";
  dump (pair, pair);
  dump 3.14;
  dump [| 1.5; 2.5; 3.5 |];
  dump [1; 2; 3];
  dump "";
  dump (-5);
  dump (-1000);
  dump 100000;
  dump (1 lsl 40);
  dump (String.make 100 'x');
  dump (String.make 300 'y');
  dump (D "payload");
  dump [||];
  dump ([1; 2; 3], "mid", 2.25);
  close_out out
