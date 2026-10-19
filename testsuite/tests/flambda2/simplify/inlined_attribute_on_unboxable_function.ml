(* TEST
   flambda;
   setup-ocamlopt.byte-build-env;
   ocamlopt_flags += " -flambda2-inline-small-function-size 0 -flambda2-inline-threshold -100";
   { ocamlopt_flags += " -stubs-forward-inlining";
     fexpr_reference_suffix = "stubs-forward-inlining.reference";
     ocamlopt.byte with dump-simplify;
     check-fexpr-dump; }
   { ocamlopt_flags += " -no-stubs-forward-inlining";
     fexpr_reference_suffix = "no-stubs-forward-inlining.reference";
     ocamlopt.byte with dump-simplify;
     check-fexpr-dump; }
 *)

let helper x y = x + y

(* [f] becomes a stub that calls its unboxed version [f_unboxed]. *)
let f x y = (x, helper x y)
[@@unboxable]

(* The [@inlined] attribute gets placed on the unboxed stub.

   When stubs forward inlining, the call to the unboxed function inside the
   stub gets marked with [@inlined forward], ensuring that the user-provided
   attribute gets forwarded to the unboxed function.

   Note that the call to [helper] in [f] should *NOT* be inlined. *)
let h =
  let a, b = (f [@inlined]) 0 0 in
  a + b
