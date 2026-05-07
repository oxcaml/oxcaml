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

let to_arity_exn ?message t =
  let message =
    match message with
    | Some message -> message
    | None -> "Expected a concrete result arity, not [Unknown] or [Bottom]"
  in
  Or_unknown_or_bottom.ok_exn t ~message

let unarized_components_or_empty t =
  Or_unknown_or_bottom.value_map t ~unknown:[] ~bottom:[]
    ~f:Flambda_arity.unarized_components
