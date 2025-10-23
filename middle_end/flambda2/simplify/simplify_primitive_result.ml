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

[@@@ocaml.warning "+a-30-40-41-42"]

type t =
  | Simplified of
      { simplified_named : Simplified_named.t Or_invalid.t;
        try_reify : bool;
        dacc : Downwards_acc.t
      }
  | Rewritten of (Flambda.Expr.t -> Flambda.Expr.t)

let create named ~try_reify dacc =
  let machine_width = Downwards_env.machine_width (Downwards_acc.denv dacc) in
  Simplified
    { simplified_named = Ok (Simplified_named.create ~machine_width named);
      try_reify;
      dacc
    }

let create_simplified simplified_named ~try_reify dacc =
  Simplified { simplified_named = Ok simplified_named; try_reify; dacc }

let create_invalid dacc =
  Simplified { simplified_named = Invalid; try_reify = false; dacc }

let create_unit dacc ~result_var ~original_term =
  (* CR gbury: would it make sense to have a Flambda2_types.unit instead of this
     ? *)
  let machine_width = Downwards_env.machine_width (Downwards_acc.denv dacc) in
  let ty =
    Flambda2_types.this_tagged_immediate (Target_ocaml_int.zero machine_width)
  in
  let dacc = Downwards_acc.add_variable dacc result_var ty in
  create original_term ~try_reify:false dacc

let create_unknown dacc ~result_var kind ~original_term =
  let ty = Flambda2_types.unknown kind in
  let dacc = Downwards_acc.add_variable dacc result_var ty in
  create original_term ~try_reify:false dacc

let create_rewrite f = Rewritten f

let is_invalid = function
  | Simplified { simplified_named = Invalid; _ } -> true
  | Simplified { simplified_named = Ok _; _ } | Rewritten _ -> false

let map_dacc t f =
  match t with
  | Simplified t -> Simplified { t with dacc = f t.dacc }
  | Rewritten _ -> t
