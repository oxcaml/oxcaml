(* TEST
   compile_only = "true";
   flambda2;

   ocamlopt_flags += " -O3";
   ocamlopt_flags += " -flambda2-inline-small-functor-size 0";
   ocamlopt_flags += " -flambda2-inline-large-functor-size 1";
   ocamlopt_flags += " -w +223";

   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 *)

(* With the large functor size forced to 1, the body of [F] is deemed too large
   for it to be eligible for inlining at the point of its definition, so the
   application of [F] is discarded before speculative inlining.  Warning 223
   should still be reported for it. *)

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
