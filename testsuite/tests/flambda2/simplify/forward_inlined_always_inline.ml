(* TEST
   compile_only = "true";
   flambda2;
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-raw, dump-simplify;
   check-fexpr-dump;
 *)

let[@inline always] f x =
  x

let[@inline always] g x =
  (* Expected behaviour: [f] is not inlined.

     In the code of [g] itself, [f] should not be inlined (because there is no
     inlining of [g] to forward). *)
  (f [@inlined forward]) x

let call_g_with_default_inlined x =
  (* Expected behaviour: [g] and [f] are inlined.

     [g] should be inlined due to its [@inline always] attribute, and the call
     to [f] within [g] should use the default inlining logic, which is to
     inline due to the [@inline always] attribute on [f]. *)
  g x

let call_g_with_always_inlined x =
  (* Expected behaviour: [g] and [f] are inlined.

     [g] should be inlined due to its [@inline always] attribute, and the
     [@inlined always] annotation propagated to the call to [f] within [g],
     which should inline [f]. *)
  (g [@inlined always]) x

let call_g_with_never_inlined x =
  (* Expected behaviour: [g] is not inlined.

     The [@inlined never] attribute applies to [g]. *)
  (g [@inlined never]) x
