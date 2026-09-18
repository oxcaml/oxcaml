(* TEST *)

[@@@ocaml.flambda_o3]

(* This function, when inlined, triggers a bug where the variable [s]
   becomes unbound. The key mechanism:

   1. [g] has two matches on [r]. During [g]'s body simplification
      (inside the function, NOT at unit toplevel), the second match's
      [Untag_immediate] let is CSE'd to the first match's result and
      deleted (since it's not at unit toplevel, no constant placement).

   2. When [g] is inlined at the call site, [g]'s simplified body is
      used - which does NOT have the second [Untag_immediate] let.

   3. At the call site, [s] is defined at unit toplevel. [Make_block(s)]
      (for [Some s]) is inside a switch handler NOT at unit toplevel.
      The block is lifted. The lifted constant references [s].

   4. There is no [Let] at unit toplevel between [let s] and the switch
      (the [Untag_immediate] was eliminated in step 1). So the lifted
      constant bubbles up to [let s].

   5. At [let s], the [has_uses] check sees [s] is not in
      [name_occurrences] (the block was lifted, removing [s] from the
      code). The binding is marked for deletion. Then the lifted constant
      is placed (adding [s] to name_occurrences), but the deletion
      decision was already made. [s] is unbound. *)
let[@inline always] g r q =
  let y = match r with
    | 0 -> q
    | 1 -> q + 1
    | _ -> q + r
  in
  let s = y + 1 in
  match r with
  | 0 -> None
  | _ -> Some s

let x =
  let r = (Random.int [@inlined never]) 3 in
  let q = (Random.int [@inlined never]) 100 in
  g r q
