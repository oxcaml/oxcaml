(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  native;
*)

#syntax quotations on

let cell = ref 0

let cell_read = Eval.eval <[ !Eval_before_init_ok.cell ]>

let () = cell := !cell + 1

let () =
  print_string
    "If this value is 0, the eval occurred before initialization finished.";
  print_newline ();
  print_int cell_read;
  print_newline ();
;;
