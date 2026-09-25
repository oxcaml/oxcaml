(* TEST
   compile_only = "true";
   flambda2;

   ocamlopt_flags += " -flambda2-inline-small-functor-size 0";
   ocamlopt_flags += " -flambda2-inline-threshold 0";
   ocamlopt_flags += " -no-flambda2-speculative-inlining-only-if-arguments-useful";

   {
     ocamlopt_flags += " -flambda2-speculative-inlining-track-lifted-constants-for-functors";
     setup-ocamlopt.byte-build-env;
     ocamlopt.byte with dump-simplify;
     check-fexpr-dump;
   }{
     ocamlopt_flags += " -flambda2-speculative-inlining-track-lifted-constants-for-functions";
     ocamlopt_flags += " -flambda2-speculative-inlining-track-lifted-constants-for-functors";
     setup-ocamlopt.byte-build-env;
     ocamlopt.byte with dump-simplify;
     check-fexpr-dump;
   }{
     ocamlopt_flags += " -flambda2-speculative-inlining-track-lifted-constants-for-functors";
     ocamlopt_flags += " -flambda2-speculative-inlining-track-lifted-constants-for-functions";
     ocamlopt_flags += " -no-flambda2-speculative-inlining-track-lifted-constants-for-functors";
     fexpr_reference_suffix = "untracked.reference";
     setup-ocamlopt.byte-build-env;
     ocamlopt.byte with dump-simplify;
     check-fexpr-dump;
   }{
     fexpr_reference_suffix = "untracked.reference";
     setup-ocamlopt.byte-build-env;
     ocamlopt.byte with dump-simplify;
     check-fexpr-dump;
   }
 *)

[@@@ocaml.flambda_o3]

(* Counting lifted constants should prevent inlining both at toplevel and
   inside a function. Only the functor flag should affect applications of [F]. *)

module F (X : sig val x : int end) = struct
  let pair = X.x, X.x
end

module Zero = struct let x = Sys.opaque_identity 0 end

module V0 = F (Zero)

module One = struct let x = Sys.opaque_identity 1 end

let v1 () =
  let module V1 = F (One) in
  V1.pair
