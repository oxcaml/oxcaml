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

type action

val bind_iterator :
  'a option Channel.receiver with_name -> 'a Trie.Iterator.t with_name -> action

val unless :
  ('t, 'k, 'v) Table.Id.t ->
  't Channel.receiver ->
  'k Option_receiver.hlist with_names ->
  action

val unless_eq :
  'k Value.repr ->
  'k option Channel.receiver with_name ->
  'k option Channel.receiver with_name ->
  action

val filter :
  ('k Constant.hlist -> bool) -> 'k Option_receiver.hlist with_names -> action

type actions

val add_action : actions -> action -> unit

module Order : sig
  type t

  val compare : t -> t -> int

  val parameters : t
end

module Level : sig
  type cardinality =
    | All_values
    | Any_value

  type 'a t

  val print : Format.formatter -> 'a t -> unit

  (** Returns a reference to the current value at this level.

      The [cardinality] argument can be used to indicate whether all the values
      of the output are needed ([All_values], the default), or if a single value
      is enough ([Any_value]). If [Any_value] is provided, the runtime makes no
      guarantees regarding the value that will actually be returned, and all
      values can be returned if there is another use with of the level with
      [All_values] cardinality or the structure of the program prevents early
      exits. As such, the [Any_value] cardinality should only be used when the
      value is solely used for debugging purposes, and never when it has a
      semantic impact on the evaluation.

      {b Note}: This reference is set to any new value found prior to executing
      the associated actions, if any, and can thus be used in actions for this
      level or levels of later orders. *)
  val use_output :
    ?cardinality:cardinality -> 'a t -> 'a option Channel.receiver with_name

  (** Actions to execute immediately after a value is found at this level. *)
  val actions : 'a t -> actions

  val add_iterator : 'a t -> 'a Trie.Iterator.t with_name -> unit

  (** Order of this level. Levels will be iterated over in a nested loop of
      ascending order: if level [order b >= order a], then the loop for [b] is
      nested {b inside} the loop for [a]. *)
  val order : 'a t -> Order.t
end

type context

val create_context : unit -> context

val add_new_level : context -> string -> 'a Level.t

val add_iterator :
  context -> ('t, 'k, 'v) Table.Id.t -> 'k Trie.Iterator.hlist with_names

val add_naive_binder : context -> ('t, 'k, 'v) Table.Id.t -> 't Channel.receiver

(** Initial actions are always executed when iterating over a cursor, before
    opening the first level. *)
val initial_actions : context -> actions

type 'v t

type 'a cursor = 'a t

val print : Format.formatter -> 'a t -> unit

type call

val create_call :
  ('c -> 'a Constant.hlist -> unit) ->
  name:string ->
  context:'c ->
  'a Option_receiver.hlist with_names ->
  call

val create :
  ?calls:call list ->
  ?output:'v Option_receiver.hlist with_names ->
  context ->
  'v t

val naive_fold :
  'v t -> Table.Map.t -> ('v Constant.hlist -> 'a -> 'a) -> 'a -> 'a

val naive_iter : 'v t -> Table.Map.t -> ('v Constant.hlist -> unit) -> unit

(** Run a [cursor] using seminaive evaluation.

    Seminaive evaluation aims at iterating over the {b new} outputs of the query
    obtained by incrementally updating the database.

    [previous] represents the old state of the database -- outputs derived
    only from facts in [previous] are not found by seminaive evaluation.

    [current] represents the new state of the database, obtained by adding the
    [diff] to [previous]. We are only interested in outputs derived from at
    least one (but maybe more than one) fact in [diff].

    Seminaive evaluation is built on the bilinearity of the join operator with
    respect to the database concatenation operator [+].
    Suppose that we have a binary query on [P] and [Q]; the output is computed
    by iterating over [join(P, Q)]. If [P = P + ΔP] and [Q = P + ΔQ], we can
    rewrite:

    {v
    join(P + ΔP, Q + ΔQ) = join(P, Q) + join(ΔP, Q) + join(P + ΔP, ΔQ)
    v}

    Seminaive evaluation ignores the [join(P, Q)] term and only computes the
    last two terms. Note that the term [join(P + ΔP, ΔQ)] does not need to be
    further decomposed, so that in the general case we only need to combine
    linearly many terms of the form:

    {v
    join(P₁ + ΔP₁, …, Pᵢ-₁ + ΔPᵢ-₁, ΔPᵢ, Pᵢ+₁, …, Pₙ
    v}

    The terms on the left use the [current] databse, the middle term uses the
    [diff] database, and the terms on the right use the [previous] database.
*)
val seminaive_run :
  'v t ->
  previous:Table.Map.t ->
  diff:Table.Map.t ->
  current:Table.Map.t ->
  unit

module With_parameters : sig
  type ('p, !'v) t

  val print : Format.formatter -> ('p, 'v) t -> unit

  val without_parameters : (nil, 'v) t -> 'v cursor

  val create :
    parameters:'p Option_sender.hlist ->
    ?calls:call list ->
    ?output:'v Option_receiver.hlist with_names ->
    context ->
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
