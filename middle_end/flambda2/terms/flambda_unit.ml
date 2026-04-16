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
  { return_continuation : Continuation.t;
    exn_continuation : Continuation.t;
    toplevel_my_region : Variable.t;
    toplevel_my_ghost_region : Variable.t;
    body : Flambda.Expr.t;
    module_symbol : Symbol.t;
    used_value_slots : Value_slot.Set.t Or_unknown.t;
    weak_symbols : Symbol.Set.t;
    weak_code_ids : Code_id.Set.t
  }

let create ~return_continuation ~exn_continuation ~toplevel_my_region
    ~toplevel_my_ghost_region ~body ~module_symbol ~used_value_slots
    ~weak_symbols ~weak_code_ids =
  { return_continuation;
    exn_continuation;
    toplevel_my_region;
    toplevel_my_ghost_region;
    body;
    module_symbol;
    used_value_slots;
    weak_symbols;
    weak_code_ids
  }

let return_continuation t = t.return_continuation

let exn_continuation t = t.exn_continuation

let toplevel_my_region t = t.toplevel_my_region

let toplevel_my_ghost_region t = t.toplevel_my_ghost_region

let body t = t.body

let module_symbol t = t.module_symbol

let used_value_slots t = t.used_value_slots

let with_used_value_slots t used_value_slots =
  { t with used_value_slots = Known used_value_slots }

let with_body t body = { t with body }

let weak_symbols t = t.weak_symbols

let weak_code_ids t = t.weak_code_ids

let [@ocamlformat "disable"] print ppf
      { return_continuation; exn_continuation; toplevel_my_region;
        toplevel_my_ghost_region; body; module_symbol; used_value_slots;
        weak_symbols; weak_code_ids;
      } =
  Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(module_symbol@ %a)@]@ \
        @[<hov 1>(return_continuation@ %a)@]@ \
        @[<hov 1>(exn_continuation@ %a)@]@ \
        @[<hov 1>(toplevel_my_region@ %a)@]@ \
        @[<hov 1>(toplevel_my_ghost_region@ %a)@]@ \
        @[<hov 1>(used_value_slots@ %a)@]@ \
        @[<hov 1>(weak_symbols@ %a)@]@ \
        @[<hov 1>(weak_code_ids@ %a)@]@ \
        @[<hov 1>%a@]\
      )@]"
    Symbol.print module_symbol
    Continuation.print return_continuation
    Continuation.print exn_continuation
    Variable.print toplevel_my_region
    Variable.print toplevel_my_ghost_region
    (Or_unknown.print Value_slot.Set.print) used_value_slots
    Symbol.Set.print weak_symbols
    Code_id.Set.print weak_code_ids
    Flambda.Expr.print body
