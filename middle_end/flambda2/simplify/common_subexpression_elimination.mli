(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Maintenance of environments and associated calculations for common
    subexpression elimination, performed during Simplify. *)

module EPA = Continuation_extra_params_and_args
module P = Flambda_primitive
module RI = Apply_cont_rewrite_id
module T = Flambda2_types
module TE = Flambda2_types.Typing_env
module TEE = Flambda2_types.Typing_env_extension

type t

val print : Format.formatter -> t -> unit

val empty : t

(** If the [t] already has an equation for the given primitive, then [add] does
    nothing. (Expected usage is that this will correspond to outermost bindings
    taking precedence, but for simplicity, this function does not enforce that.)
*)
val add : t -> P.Eligible_for_cse.t -> bound_to:Simple.t -> Scope.t -> t

val find : t -> P.Eligible_for_cse.t -> Simple.t option

(** Discard all CSE equations whose left-hand side is a primitive that has
    coeffects. This should be called at program points where coeffects might be
    observed differently across the relevant primitive's prior occurrence and
    its later (potentially CSEd) occurrence: across a function call, across a
    primitive that has arbitrary effects, or across an allocation on the OCaml
    heap (heap allocation points can run arbitrary code, for example finalizers
    and signal handlers; allocations on the local allocation stack cannot). *)
val clear_equations_on_coeffectful_primitives : t -> t

module Join_result : sig
  type nonrec t = private
    { cse_at_join_point : t;
      extra_params : EPA.t;
      env_extension : TEE.t;
      extra_allowed_names : Name_occurrences.t
    }
end

(** [join] adds CSE equations into [cse_at_fork] at the next scope level after
    that given by the [typing_env_at_fork]. The returned [cse_at_join_point]
    must always be used as the CSE state for the join point: equations on
    coeffectful primitives that were killed on some path to the join (or, for
    recursive continuations, all such equations) have been removed from it. *)
val join :
  typing_env_at_fork:TE.t ->
  cse_at_fork:t ->
  is_recursive:bool ->
  use_info:'a list ->
  get_typing_env:('a -> TE.t) ->
  get_rewrite_id:('a -> RI.t) ->
  get_cse:('a -> t) ->
  params:Bound_parameters.t ->
  Join_result.t
