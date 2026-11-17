(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming -I ${ocamlsrcdir}/utils";
  no-poll-insertion;
  native;
*)

#syntax quotations on

(* Evaluated code is loaded by the JIT, which takes it from the native
   backend as that backend emits it.  The LLVM backend writes an object
   file instead, so evaluation must refuse it, and say that it is the
   backend to change: in this executable, which is not fully polled, the
   code would otherwise be admitted (see poll_refused.ml for one that
   is). *)

let () =
  Clflags.llvm_backend := true;
  match (Eval.eval <[ 1 + 2 ]> : int) with
  | n -> Printf.printf "unexpectedly evaluated to %d\n" n
  | exception Failure message -> print_endline message
;;
