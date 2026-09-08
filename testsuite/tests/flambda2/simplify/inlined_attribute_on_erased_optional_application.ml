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

let default x = x

let f ?(x = default 0) y z = x + y + z

(* [g] is a partial application stub:

   ```
   let[@stub] g z = f ?x:None 0 z
   ```
 *)
let g = f 0

(* The [@inlined] attribute gets placed on the partial application stub.

   When stubs forward inlining, the call to [f] inside the stub is marked with
   [@inlined forward], ensuring that the user-provided attribute gets forwarded
   to [f].

   Note that the call to [default] in the optional default argument of [f]
   should *NOT* be inlined. *)
let h = (g [@inlined]) 0
