(* TEST
   flambda;
   setup-ocamlopt.byte-build-env;
   ocamlopt_flags += " -flambda2-inline-small-functor-size 0 -flambda2-inline-threshold -100 -no-flambda2-result-types";
   { ocamlopt_flags += " -stubs-forward-inlining";
     fexpr_reference_suffix = "stubs-forward-inlining.reference";
     ocamlopt.byte with dump-simplify;
     check-fexpr-dump; }
   { ocamlopt_flags += " -no-stubs-forward-inlining";
     fexpr_reference_suffix = "no-stubs-forward-inlining.reference";
     ocamlopt.byte with dump-simplify;
     check-fexpr-dump; }
 *)

module type S = sig val x : int end

module F : sig
  module Make (X : S) : S
end = struct
  (* No signature here to force a coercion to the signature of [F].

     [F.Make] will be a stub that calls [Make] and rearranges the result into a
     block with a single field. *)
  module Make(X : S) = struct
    let _dummy = ()
    let x = X.x
  end
end

(* The [@inlined] attribute gets placed on the coercion stub. 

   When stubs forward inlining, the call to [Make] inside the stub is marked
   with [@inlined forward], ensuring that the user-provided attribute gets
   forwarded to [Make]. *)
module M = (F.Make [@inlined])(struct let x = 0 end)
