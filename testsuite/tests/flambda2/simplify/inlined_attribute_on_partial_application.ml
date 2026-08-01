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

let f x y = x + y

(* [g] is a partial application stub:

   ```
   let[@stub] g y = f 0 y
   ```
 *)
let g = f 0

(* The [@inlined] attribute gets placed on the partial application stub.

   When stubs forward inlining, the call to [f] inside the stub is marked with
   [@inlined forward], ensuring that the user-provided attribute gets forwarded
   to [f]. *)
let h = (g [@inlined]) 0
