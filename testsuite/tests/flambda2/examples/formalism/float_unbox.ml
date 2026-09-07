(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Formalism validation: a [float ref] accumulator loop drives the recursive
   loop continuation to a naked-float parameter, dissolving the ref.
   Rules: S.Unbox.ContParam.Rewrite, S.Unbox.Mutable.Rewrite,
   S.Unbox.Optimistic.Number.
   Case study: middle_end/flambda2/docs/formalism/14-validation/new-07-float-unbox.md
   Phenomenon: loop continuation with a naked-float param; the ref dissolved. *)

let f () =
  let r = ref 0. in
  for i = 1 to 1000 do r := !r +. float i done;
  !r
