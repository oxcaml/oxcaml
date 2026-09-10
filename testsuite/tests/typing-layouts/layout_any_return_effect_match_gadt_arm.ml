(* TEST
 flags = "-extension layouts_beta -keywords 5.3";
 flambda2;
 {
   native;
 }
*)

(* A match-with-effect-handlers whose result is refined to a concrete value
   layout by a GADT arm is accepted (its result has layout value) and must
   compile: the value result flows through the enclosing function's
   unknown-arity ([any]) return continuation.  Flambda2 simplify must not
   collapse the fixed-arity effect-result continuation onto the unknown-arity
   return continuation, which would fail with "Unknown-arity function return
   continuation cannot be used where a fixed arity is required". *)

type _ Effect.t += E : int Effect.t

type (_ : any) w = Int : int w

let[@inline never] no_effect : type (a : any). a w -> a = function
  | Int -> (match 1 with x -> x | effect E, k -> Effect.Deep.continue k 41)

let[@inline never] with_effect : type (a : any). a w -> a = function
  | Int ->
    (match Effect.perform E + 1 with
     | x -> x
     | effect E, k -> Effect.Deep.continue k 41)

let () =
  assert (no_effect Int = 1);
  assert (with_effect Int = 42)
