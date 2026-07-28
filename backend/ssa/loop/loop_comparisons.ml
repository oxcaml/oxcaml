[@@@ocaml.warning "+a-4-40-41-42-44"]

(* See [loop_comparisons.mli] for the interface. *)

module Affine = Fourier_motzkin.Affine

type direction =
  | Up
  | Down

let direction_of_step step =
  if step > 0 then Some Up else if step < 0 then Some Down else None

let is_signed_order (cmp : Cmm.integer_comparison) =
  match cmp with
  | Clt | Cle | Cgt | Cge -> true
  | Ceq | Cne | Cult | Cugt | Cule | Cuge -> false

let oriented_continue_comparison ~iv_is_left ~continue_when_true
    (cmp : Cmm.integer_comparison) : Cmm.integer_comparison =
  let cmp = if iv_is_left then cmp else Cmm.swap_integer_comparison cmp in
  if continue_when_true then cmp else Cmm.negate_integer_comparison cmp

(* Continue-condition (oriented with the IV operand on the left): the loop
   continues iff this comparison holds. Decides whether monotonic progression in
   [dir] must eventually break the comparison. We only handle signed
   comparisons; unsigned wrap-around invalidates monotonicity, so those are
   reported as non-terminating (i.e. unknown). *)
let continue_terminates dir (cmp : Cmm.integer_comparison) =
  match dir, cmp with
  | Up, (Clt | Cle) -> true
  | Down, (Cgt | Cge) -> true
  | (Up | Down), Ceq -> true
  | Up, (Cgt | Cge | Cne | Cult | Cugt | Cule | Cuge)
  | Down, (Clt | Cle | Cne | Cult | Cugt | Cule | Cuge) ->
    false

let continues_while_upper_bounded (cmp : Cmm.integer_comparison) =
  match cmp with
  | Clt | Cle -> true
  | Ceq | Cne | Cgt | Cge | Cult | Cugt | Cule | Cuge -> false

(* Facts implied by the (possibly negated) signed comparison [la cmp lb].
   Unsigned comparisons and [Cne] cannot be expressed as a single affine
   inequality, so they contribute nothing. *)
let facts ~negate (cmp : Cmm.integer_comparison) la lb : Affine.t list =
  let cmp = if negate then Cmm.negate_integer_comparison cmp else cmp in
  match cmp with
  | Cge -> [Affine.sub la lb]
  | Cgt -> [Affine.add_const (Affine.sub la lb) (-1)]
  | Cle -> [Affine.sub lb la]
  | Clt -> [Affine.add_const (Affine.sub lb la) (-1)]
  | Ceq -> [Affine.sub la lb; Affine.sub lb la]
  | Cne | Cult | Cugt | Cule | Cuge -> []
