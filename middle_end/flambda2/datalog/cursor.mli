(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2024--2025 OCamlPro SAS                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Datalog_imports

type 'v t

type 'a cursor = 'a t

val print : Format.formatter -> 'a t -> unit

val naive_fold :
  'v t -> Table.Map.t -> ('v Constant.hlist -> 'a -> 'a) -> 'a -> 'a

val naive_iter : 'v t -> Table.Map.t -> ('v Constant.hlist -> unit) -> unit

(** Run a [cursor] using seminaive evaluation.

    Seminaive evaluation aims at iterating over the {b new} outputs of the query
    obtained by incrementally updating the database.

    [previous] represents the old state of the database -- outputs derived only
    from facts in [previous] are not found by seminaive evaluation.

    [current] represents the new state of the database, obtained by adding the
    [diff] to [previous]. We are only interested in outputs derived from at
    least one (but maybe more than one) fact in [diff].

    Seminaive evaluation is built on the bilinearity of the join operator with
    respect to the database concatenation operator [+]. Suppose that we have a
    binary query on [P] and [Q]; the output is computed by iterating over
    [join(P, Q)]. If [P = P + ΔP] and [Q = P + ΔQ], we can rewrite:

    {v join(P + ΔP, Q + ΔQ) = join(P, Q) + join(ΔP, Q) + join(P + ΔP, ΔQ) v}

    Seminaive evaluation ignores the [join(P, Q)] term and only computes the
    last two terms. Note that the term [join(P + ΔP, ΔQ)] does not need to be
    further decomposed, so that in the general case we only need to combine
    linearly many terms of the form:

    {v join(P₁ + ΔP₁, …, Pᵢ-₁ + ΔPᵢ-₁, ΔPᵢ, Pᵢ+₁, …, Pₙ v}

    The terms on the left use the [current] databse, the middle term uses the
    [diff] database, and the terms on the right use the [previous] database. *)
val seminaive_run :
  'v t ->
  previous:Table.Map.t ->
  diff:Table.Map.t ->
  current:Table.Map.t ->
  unit

type binder =
  | Bind_table : ('t, 'k, 'v) Table.Id.t * 't Channel.or_null_sender -> binder

module With_parameters : sig
  type ('p, !'v) t

  val print : Format.formatter -> ('p, 'v) t -> unit

  val without_parameters : (nil, 'v) t -> 'v cursor

  val create_from_rule :
    ?callback:('v Constant.hlist -> unit) ref ->
    'p Lang.Variable.hlist ->
    _ Lang.Variable.hlist ->
    Lang.rule ->
    ('p, 'v) t

  val naive_fold :
    ('p, 'v) t ->
    'p Constant.hlist ->
    Table.Map.t ->
    ('v Constant.hlist -> 'a -> 'a) ->
    'a ->
    'a

  val naive_iter :
    ('p, 'v) t ->
    'p Constant.hlist ->
    Table.Map.t ->
    ('v Constant.hlist -> unit) ->
    unit

  val seminaive_run :
    ('p, 'v) t ->
    'p Constant.hlist ->
    previous:Table.Map.t ->
    diff:Table.Map.t ->
    current:Table.Map.t ->
    unit
end
