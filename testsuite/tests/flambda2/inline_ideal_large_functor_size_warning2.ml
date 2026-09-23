(* TEST
   compile_only = "true";
   flambda2;

   ocamlopt_flags += " -O3";
   ocamlopt_flags += " -flambda2-inline-small-functor-size 0";
   ocamlopt_flags += " -no-flambda2-speculative-inlining-only-if-arguments-useful";
   ocamlopt_flags += " -flambda2-inline-large-functor-size 1";
   ocamlopt_flags += " -flambda2-inline-ideal-large-functor-size 2000000";
   ocamlopt_flags += " -w +222";

   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 *)

[@@@ocaml.flambda_o3]

(* With the large functor size forced to 1, the body of [F] is deemed too
   large for it to be eligible for inlining.  With the ideal large functor
   size of 2000000, the application of [F] would instead be subject to
   speculative inlining, which would decide to inline it, so warning 222
   should be reported at the application of [F]. *)

module type S = sig
  val x : int
end

module F (M : S) = struct
  let a = M.x + 1

  let b = a * 2

  let c = b + a

  let d = c * c
end

module A = F (struct
  let x = Sys.opaque_identity 1
end)

let use () = A.d
