(* TEST compile_only = "true"; flambda2; ocamlopt_flags = "-O3 -dcmm -dcanonical-ids";
   setup-ocamlopt.byte-build-env; ocamlopt.byte; check-ocamlopt.byte-output;
*)

(* to_cmm formalism validation: a try/with lowers to an exception-handler catch with trap
   push/pop, a reraise, and a surrounding local region. Rules: TC.LetCont.Exn,
   TC.ApplyCont.Raise, TC.ApplyCont.Jump (ch. 16), CM.Catch.Exn, CM.Exit.Trap, CM.Raise,
   CM.Region.Begin/End (ch. 15, 19), INV.ToCmm.Control (ch. 16). Case study:
   middle_end/flambda2/docs/formalism/14-validation/tocmm-03-trywith-region.md *)

let[@inline never] tw g x =
  try g x with
  | Not_found -> -1
;;
