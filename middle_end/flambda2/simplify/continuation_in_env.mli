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

type t =
  | Linearly_used_and_inlinable of
      { params : Bound_parameters.t;
            (** To avoid re-opening name abstractions, we store the opened
                parameters and handler here. *)
        handler : Rebuilt_expr.t
      }
  | Non_inlinable_zero_arity of
      { handler : Rebuilt_expr.t Or_unknown.t
            (** The handler, if available, is stored for [Simplify_switch_expr].
            *)
      }
  | Non_inlinable_non_zero_arity of { arity : [`Unarized] Flambda_arity.t }
  | Toplevel_or_function_return_or_exn_continuation of
      { arity : [`Unarized] Flambda_arity.t }
  | Invalid of { arity : [`Unarized] Flambda_arity.t }
      (** [Invalid] means that the code of the continuation handler is invalid,
          not that the continuation has zero uses. *)

val print : Are_rebuilding_terms.t -> Format.formatter -> t -> unit

val arity : t -> [`Unarized] Flambda_arity.t
