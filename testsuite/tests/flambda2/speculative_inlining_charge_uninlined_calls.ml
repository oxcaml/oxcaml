(* TEST
   compile_only = "true";
   flambda2;

   ocamlopt_flags += " -flambda2-speculative-inlining-track-lifted-constants";
   ocamlopt_flags += " -flambda2-speculative-inlining-charge-uninlined-calls";

   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-simplify;
   check-fexpr-dump;
 *)

[@@@ocaml.flambda_o3]

(* Speculative inlining does not perform nested speculation: inside a
   speculatively-inlined body, calls to speculatively-inlinable functions are
   left as calls.  The size measured by the speculation therefore did not
   account for any inlining of such calls that would happen once the body was
   really inlined.  In the example below, each level of [f] appeared cheap to
   inline in isolation, but the closures duplicated by each inlining contain
   three calls to the level below, so the final code contained 3^n copies of
   [big] (where [n] is the number of wrapper levels).

   With [-flambda2-speculative-inlining-charge-uninlined-calls], the code size
   of each such uninlined call is charged to the speculation, which prevents
   the exponential blow-up: the top-level call [f 1] should produce only one
   copy of [big]. *)

let[@inline never] call ~f = f ()

let f x =
  let big () =
    match x with
    | 1 ->
       [| x; x; x; x; x; x; x; x; x; x;
          x; x; x; x; x; x; x; x; x; x;
          x; x; x; x; x; x; x; x; x; x;
          x; x; x; x; x; x; x; x; x; x;
          x; x; x; x; x; x; x; x; x; x; |].(7)
    | 2 ->
       [| x; x; x; x; x; x; x; x; x; x;
          x; x; x; x; x; x; x; x; x; x;
          x; x; x; x; x; x; x; x; x; x;
          x; x; x; x; x; x; x; x; x; x;
          x; x; x; x; x; x; x; x; x; x; |].(9)
    | _ ->
       [| x; x; x; x; x; x; x; x; x; x;
          x; x; x; x; x; x; x; x; x; x;
          x; x; x; x; x; x; x; x; x; x;
          x; x; x; x; x; x; x; x; x; x;
          x; x; x; x; x; x; x; x; x; x; |].(3)
  in
  call ~f:big

let f x = call ~f:(fun () -> f x + f x + f x)
let f x = call ~f:(fun () -> f x + f x + f x)
let f x = call ~f:(fun () -> f x + f x + f x)

let g () = f 1
