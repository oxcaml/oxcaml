(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  native;
*)

(* This test relies on the initialisations happening in order and
   writing results to the module items at the right address.
   This does not seem to happen in [-Oclassic] (where this test is disabled)
   and might be inconsistent in general until we address the underlying issue.
   Before initialisation, values should be set to a tagged 0 (0x1). *)

(* CR metaprogramming jbachurski: Eval running on a quote that refers to
   the currently initalised unit should be made well-behaved.
   Internal ticket 7260. *)

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
