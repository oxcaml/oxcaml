(* TEST
   compile_only = "true";
   flambda2;

   ocamlopt_flags += " -O3";
   ocamlopt_flags += " -flambda2-inline-small-functor-size 0";
   ocamlopt_flags += " -no-flambda2-speculative-inlining-only-if-arguments-useful";
   ocamlopt_flags += " -w +223";

   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 *)

(* The application of [F] is subject to speculative inlining, so warning 223
   should report the decision taken by that process, including the cost
   metrics and the evaluated cost. *)

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
