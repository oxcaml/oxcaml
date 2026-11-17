(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming -I ${ocamlsrcdir}/utils";
  native;
*)

#syntax quotations on

(* Evaluated code is compiled in-process and runs in this executable's
   image, so [Eval.eval] must refuse to compile for a codegen variant other
   than the one the executable was built for.  Flipping poll insertion is a
   harmless way to select a different variant. *)

let () =
  Clflags.poll_insertion := not !Clflags.poll_insertion;
  match (Eval.eval <[ 1 + 2 ]> : int) with
  | n -> Printf.printf "unexpectedly evaluated to %d\n" n
  | exception Failure _ -> print_endline "rejected a mismatched variant"
;;
