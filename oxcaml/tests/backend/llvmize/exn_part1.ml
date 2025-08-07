exception Exn1 of int

exception Exn2

let ref1 = ref 0

let ref2 = ref 0

let[@inline never] [@local never] raise_exn1_from_ocaml n : unit =
  (* print_endline "raise Exn1"; *)
  raise (Exn1 n)

let[@inline never] [@local never] raise_exn1_catch_exn2_from_ocaml () =
  ref1 := 1;
  print_endline "try";
  try raise_exn1_from_ocaml 10 with Exn2 -> print_endline "cannot happen"

let[@inline never] [@local never] secret x = x + 1

(* This is because we don't have frametables now and external C calls might
   trigger the GC, which will try to look up the frametable at the callsite and
   fail. *)
let[@inline never] [@local never] my_print_int n =
  print_int n;
  print_newline ()
