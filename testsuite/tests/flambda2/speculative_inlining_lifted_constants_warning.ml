(* TEST
   compile_only = "true";
   flambda2;

   ocamlopt_flags += " -flambda2-inline-small-function-size 0";
   ocamlopt_flags += " -flambda2-inline-small-functor-size 0";
   ocamlopt_flags += " -flambda2-inline-ideal-large-functor-size 10000";
   ocamlopt_flags += " -flambda2-inline-threshold 0";
   ocamlopt_flags += " -no-flambda2-speculative-inlining-only-if-arguments-useful";
   ocamlopt_flags += " -w +222";

   {
     setup-ocamlopt.byte-build-env;
     ocamlopt.byte;
     check-ocamlopt.byte-output;
   }{
     ocamlopt_flags += " -flambda2-speculative-inlining-track-lifted-constants-for-functions";
     setup-ocamlopt.byte-build-env;
     ocamlopt.byte;
     check-ocamlopt.byte-output;
   }
 *)

[@@@ocaml.flambda_o3]

(* The ideal configuration enables lifted-constant tracking for functors only.
   Both applications of [F] should warn when functor tracking is disabled,
   regardless of the function tracking flag. The calls to [f] should never
   warn, even with an ideal large-functor size configured. *)

let f x = (x, x)

let zero = Sys.opaque_identity 0

let v0 = f zero

let one = Sys.opaque_identity 1

let v1 () = f one

module F (X : sig val x : int end) = struct
  let pair = X.x, X.x
end

module Zero = struct let x = Sys.opaque_identity 0 end

module V0 = F (Zero)

module One = struct let x = Sys.opaque_identity 1 end

let v2 () =
  let module V1 = F (One) in
  V1.pair
