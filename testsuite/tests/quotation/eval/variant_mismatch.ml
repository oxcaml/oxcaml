(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming -I ${ocamlsrcdir}/utils";
  native;
*)

#syntax quotations on

(* Evaluated code is compiled in-process and runs in this executable's
   image, so [Eval.eval] must refuse to compile for a codegen variant whose
   whole-program axes are not those the executable was built for.  Flipping
   stack checks is a harmless way to select such a variant.  Poll insertion
   is a property of each unit: code compiled with it runs in any
   executable. *)

let () =
  Clflags.poll_insertion := true;
  Printf.printf "evaluated with poll insertion: %d\n"
    (Eval.eval <[ 1 + 2 ]> : int);
  Clflags.stack_checks := not !Clflags.stack_checks;
  match (Eval.eval <[ 1 + 2 ]> : int) with
  | n -> Printf.printf "unexpectedly evaluated to %d\n" n
  | exception Failure _ -> print_endline "rejected a mismatched variant"
;;
