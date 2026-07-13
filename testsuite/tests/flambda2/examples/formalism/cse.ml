(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Formalism validation: two identical [String.length] reads of an immutable
   string collapse to one.
   Rules: S.Rewrite.CSE.Eligible, S.Rewrite.CSE.Extend, S.Rewrite.CSE.Replace.
   Case study: middle_end/flambda2/docs/formalism/14-validation/new-04-cse.md
   Phenomenon: a single %string_length (and a single %tag_imm). *)

let f s = String.length s + String.length s
