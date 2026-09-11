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

module DE = Downwards_env
module LC = Lifted_constant

type t

val empty : t

val is_empty : t -> bool

val print : Format.formatter -> t -> unit

val singleton : LC.t -> t

val add : t -> LC.t -> t

val singleton_list_of_constants : LC.t list -> t

val union : t -> t -> t

val fold : t -> init:'a -> f:('a -> LC.t -> 'a) -> 'a

val all_defined_symbols : t -> Symbol.Set.t

val add_to_denv : ?maybe_already_defined:unit -> DE.t -> t -> DE.t

(** Cost of pending definitions reachable from the final runtime roots. Newly
    specialised code is charged once; existing code and age-only ancestors are
    free. Other constants follow the inlining tracking policy. *)
val cost_metrics : t -> roots:Name_occurrences.t -> Cost_metrics.t

(** Prune a placement batch during non-rebuilding speculation, before any dead
    definition can contribute dependencies to subsequent cost roots. Age-only
    definitions are dropped too, so this is not for rebuilding real terms. *)
val retain_reachable_for_speculation : t -> roots:Name_occurrences.t -> t

type sort_result = private { innermost_first : LC.t array }

val sort : t -> sort_result
