(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = [`Unarized] Flambda_arity.t Or_unknown_or_bottom.t

let ok arity : t = Ok arity

let print ppf t = Or_unknown_or_bottom.print Flambda_arity.print ppf t

let equal_exact t1 t2 =
  Or_unknown_or_bottom.equal Flambda_arity.equal_exact t1 t2

let equal_ignoring_subkinds t1 t2 =
  Or_unknown_or_bottom.equal Flambda_arity.equal_ignoring_subkinds t1 t2

let is_singleton_value (t : t) =
  match t with
  | Ok arity -> Flambda_arity.is_one_param_of_kind_value arity
  | Unknown | Bottom -> false

let any_value_placeholder =
  Flambda_arity.create_singletons [Flambda_kind.With_subkind.any_value]

let to_arity_with_placeholder (t : t) =
  match t with Ok arity -> arity | Unknown | Bottom -> any_value_placeholder
