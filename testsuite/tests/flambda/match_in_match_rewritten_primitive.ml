(* TEST
 flambda;
 ocamlopt_flags = "-O3 -flambda2-match-in-match -flambda2-expert-cont-specialization-threshold 200";
 native;
*)

(* Continuation specialization needs the test not to be in classic mode. *)
[@@@ocaml.flambda_o3]

(* Simplify rewrites some primitives into several bindings, the first ones
   being of fresh variables. Whether it does so depends on the types of the
   arguments of the primitive.

   In the functions below, [b] is the parameter of a continuation that gets
   specialized, as its handler ends with a switch on [b] and [b] is known at
   the uses of the continuation. The types are more precise when the handler is
   simplified again for each use than they were the first time, so the
   primitive in the handler is not rewritten in the same way, and the variables
   bound in these passes differ. Continuations that use the result of the
   primitive are lifted out of the handler. *)

let[@inline never] g (x : bool) = x

(* The comparison of a boolean with a boolean constant is rewritten into an
   untagging, followed for [= false] and [<> true] by a negation, in which case
   the untagging is bound to a fresh variable. Nothing is rewritten when
   neither argument is known to be a constant, or when the result is known. *)

(* The comparison is not rewritten at first, as [b] is not known. Once [b] is
   known to be [false], it is rewritten using a fresh variable. *)
let[@inline never] rewritten_when_specialized c n =
  let b = if c then g true || true else false in
  let y = n >= 0 in
  let r = y = b in
  if b then if g r then 1 else 2 else if g r then 3 else 4

(* The comparison is rewritten using a fresh variable at first, as [b] is only
   known to be a boolean. Once [b] is known, so is the result. *)
let[@inline never] rewritten_at_first c =
  let b = if c then g true || true else false in
  let r = b = false in
  if b then if g r then 1 else 2 else if g r then 3 else 4

external globalize : local_ float -> float = "%obj_dup"

type state = { mutable truncated : int }

let state = { truncated = 0 }

let[@inline never] h (x : float) = x

(* [%obj_dup] of a boxed float that may be local is rewritten into an unboxing,
   bound to a fresh variable, followed by a boxing. It is not when a variable
   is already bound to the contents of the float, which is the case once [b] is
   known to be [true]. *)
let[@inline never] not_rewritten_when_specialized c (local_ x : float) =
  let b =
    if c
    then (
      state.truncated <- int_of_float x;
      true)
    else false
  in
  let d = globalize x in
  if b then h d +. 1. else h d -. 1.

let () =
  let check ~expected result = assert (Int.equal expected result) in
  check ~expected:1 (rewritten_when_specialized true 5);
  check ~expected:2 (rewritten_when_specialized true (-5));
  check ~expected:4 (rewritten_when_specialized false 5);
  check ~expected:3 (rewritten_when_specialized false (-5));
  check ~expected:2 (rewritten_at_first true);
  check ~expected:3 (rewritten_at_first false);
  assert (Float.equal 3.5 (not_rewritten_when_specialized true 2.5));
  check ~expected:2 state.truncated;
  assert (Float.equal 1.5 (not_rewritten_when_specialized false 2.5))
