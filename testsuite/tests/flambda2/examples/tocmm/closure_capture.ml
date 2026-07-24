(* TEST compile_only = "true"; flambda2; ocamlopt_flags = "-O3 -dcmm -dcanonical-ids";
   setup-ocamlopt.byte-build-env; ocamlopt.byte; check-ocamlopt.byte-output;
*)

(* to_cmm formalism validation: a closure capturing a variable is allocated as a
   closure_tag block (code pointer + closinfo + value slot), and the captured variable is
   read back from the closure environment. Rules: R.Obj.Closures (ch. 17),
   TC.Prim.ProjectValueSlot, TC.Let.SetOfClosures (ch. 18). Case study:
   middle_end/flambda2/docs/formalism/14-validation/tocmm-04-closure-capture.md *)

let[@inline never] adder x =
  let f y = x + y in
  f
;;
