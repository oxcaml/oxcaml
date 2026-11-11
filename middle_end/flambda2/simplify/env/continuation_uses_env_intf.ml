(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
  type t

  val record_continuation :
    t -> Continuation.t -> [`Unarized] Flambda_arity.t -> t

  val record_continuation_use :
    t ->
    Continuation.t ->
    Continuation_use_kind.t ->
    env_at_use:Downwards_env.t ->
    arg_types:Flambda2_types.t list ->
    t * Apply_cont_rewrite_id.t

  val delete_continuation_uses : t -> Continuation.t -> t

  val get_typing_env_no_more_than_one_use :
    t -> Continuation.t -> Flambda2_types.Typing_env.t option

  val num_continuation_uses : t -> Continuation.t -> int

  val all_continuations_used : t -> Continuation.Set.t
end
