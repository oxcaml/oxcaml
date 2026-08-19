(* TEST
 flags = "-extension layouts_beta -keywords 5.3";
 flambda2;
 {
   native;
 }
*)

(* A try-with-effect-handlers refined to a concrete value layout by a GADT arm
   is accepted and must compile.  [Texp_try] carries the result sort
   determined by the typechecker, and translation must use it directly:
   recomputing a layout from the enclosing function's [any] ([Ptop]) return
   type instead would crash with "Matching with effect handlers is only
   supported for scrutinees of kind [value]". *)

type _ Effect.t += E : int Effect.t

type (_ : any) w = Int : int w

let[@inline never] no_effect : type (a : any). a w -> a =
  fun w ->
    match w with
    | Int -> (try 1 with effect E, k -> Effect.Deep.continue k 41)

let[@inline never] with_effect : type (a : any). a w -> a =
  fun w ->
    match w with
    | Int ->
      (try Effect.perform E + 1 with effect E, k -> Effect.Deep.continue k 41)

let () =
  assert (no_effect Int = 1);
  assert (with_effect Int = 42)
