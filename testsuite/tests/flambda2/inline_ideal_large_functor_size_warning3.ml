(* TEST
   compile_only = "true";
   flambda2;
   readonly_files = "inline_ideal_large_functor_size_warning1.ml";
   setup-ocamlopt.byte-build-env;

   ocamlopt_flags = "-O3";
   ocamlopt_flags += " -no-flambda2-speculative-inlining-only-if-arguments-useful";
   ocamlopt_flags += " -flambda2-inline-threshold 0";
   ocamlopt_flags += " -flambda2-inline-small-functor-size 100";
   module = "inline_ideal_large_functor_size_warning1.ml";
   ocamlopt.byte;

   ocamlopt_flags += " -flambda2-inline-small-functor-size 0";
   ocamlopt_flags += " -flambda2-inline-ideal-large-functor-size 10000";
   ocamlopt_flags += " -w @222";
   module = "inline_ideal_large_functor_size_warning3.ml";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 *)

(* The imported functor is small according to its definition's arguments, in
   both the current and ideal configurations. Using the caller's small functor
   limit instead would incorrectly predict a failed speculation and warn. *)

module A = Inline_ideal_large_functor_size_warning1.F (struct
  let x = Sys.opaque_identity 1
end)

let use () = A.d
