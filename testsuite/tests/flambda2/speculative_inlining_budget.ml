(* TEST
   compile_only = "true";
   flambda2;

   ocamlopt_flags += " -flambda2-speculative-inlining-track-lifted-constants";
   ocamlopt_flags += " -flambda2-speculative-inlining-budget";

   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-simplify;
   check-fexpr-dump;
 *)

[@@@ocaml.flambda_o3]

(* Speculative inlining does not perform nested speculation: inside a
   speculatively-inlined body, calls to speculatively-inlinable functions are
   left as calls.  In the example below, each level of [f] appeared cheap to
   inline in isolation, but the closures duplicated by each inlining contain
   three calls to the level below, so the final code contained 3^n copies of
   [big] (where [n] is the number of wrapper levels).

   With [-flambda2-speculative-inlining-budget], the inlining threshold is
   treated as a budget shared by all speculative inlinings performed within a
   speculatively-inlined body, with the cost of the code produced inside that
   body being charged against the budget as simplification proceeds.  Each of
   the three inlinings of a level of [f] at the top-level call [f 1] can then
   only afford to inline one call to the level below, after which the
   remaining calls (whose code size is large compared to the remaining budget)
   are not even speculated upon.  The final code should contain four copies of
   [big] rather than 3^3 + 1. *)

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
