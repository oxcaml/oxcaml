(* TEST
 include eval;
 flags = "-extension runtime_metaprogramming -uses-metaprogramming";
 native;
*)

#syntax quotations on

let test e =
  let e = Obj.magic_many e in
  print_endline "generated program:";
  print_endline (Quote.string_of_expr e); print_newline ();
  try
    ignore (Eval.eval e)
  with e ->
    print_endline "eval failed.\n"

(* This is fine -- and the attribute should be printed  *)
let () = test <[ ($(<[42]>) [@magic_staged_modes]) ]>

(* The following two examples show unsound uses of [@magic_staged_modes]
   that cause program generation failures. *)

(* This causes an eval error -- we ignored that the quote is non-legacy (local),
   and then spliced it as legacy (global). *)
let () = test <[ ($(<[stack_ (Some 42)]> [@magic_staged_modes])) ]>

(* This causes an eval error -- we quoted something legacy (aliased),
   and then spliced it as non-legacy (unique). *)
let () =
  let x = <[ref 0]> in
  test <[ (($x [@magic_staged_modes]) : _ @ unique) ]>
