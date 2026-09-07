(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Formalism validation: a [try...with] around a call to an unknown (opaque)
   function keeps its push/pop trap actions -- they must NOT be optimized away.
   Rules: OS.ApplyCont.TrapPush, OS.ApplyCont.TrapPop.
   Case study: middle_end/flambda2/docs/formalism/14-validation/new-06-trap.md
   Phenomenon: push/pop trap actions preserved around the opaque apply. *)

let f g = try g () with _ -> 0
