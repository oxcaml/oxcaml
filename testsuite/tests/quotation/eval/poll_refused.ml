(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming -I ${ocamlsrcdir}/utils";
  poll-insertion;
  setup-ocamlopt.opt-build-env;
  ocamlopt.opt;
  check-ocamlopt.opt-output;
  ocamlrunparam = "d=2";
  run;
  check-program-output;
*)

#syntax quotations on

(* Evaluated code is compiled in-process and runs in this executable's
   image.  This executable is fully polled and may run more than one
   domain, so [Eval.eval] must refuse code compiled without poll insertion,
   and say what can be done about it.  The code of the LLVM backend is
   refused for another reason, which comes first: the JIT cannot load it,
   with or without polls and whatever the number of domains, so the
   refusal names the backend and offers nothing else. *)

let refused () =
  match (Eval.eval <[ 1 + 2 ]> : int) with
  | n -> Printf.printf "unexpectedly evaluated to %d\n" n
  | exception Failure message -> print_endline message

let () =
  Clflags.poll_insertion := false;
  refused ();
  Clflags.poll_insertion := true;
  Clflags.llvm_backend := true;
  refused ();
  Clflags.poll_insertion := false;
  refused ()
;;
