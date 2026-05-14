(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  native;
*)

#syntax quotations on

(* This file showcases a bug where we can access values from the current unit
   in a top-level [eval] before the rest of the unit is initialized. *)

let x' = Eval.eval <[ Recursively_bad.x ]>

let x = Sys.opaque_identity 42

let () =
  print_string
    "If anything except 42 is printed, \
     top-level eval that runs before a value is initialized \
     may cause undefined behaviour.";
  print_newline ();
  print_int x';
  print_newline ();
;;
