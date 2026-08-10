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

module Metadata = struct
  type t =
    { return_continuation : Continuation.t;
      exn_continuation : Continuation.t;
      toplevel_my_region : Variable.t;
      toplevel_my_ghost_region : Variable.t;
      toplevel_my_alloc_region : Variable.t;
      module_symbol : Symbol.t;
      used_value_slots : Value_slot.Set.t Or_unknown.t
    }

  let module_symbol t = t.module_symbol

  let ids_for_export
      { return_continuation;
        exn_continuation;
        toplevel_my_region;
        toplevel_my_ghost_region;
        toplevel_my_alloc_region;
        module_symbol;
        used_value_slots = _
      } =
    (* CR mvellacott: Minimise what's stored when we merge .cmr and .cmx. *)
    let ids = Ids_for_export.empty in
    let ids = Ids_for_export.add_continuation ids return_continuation in
    let ids = Ids_for_export.add_continuation ids exn_continuation in
    let ids = Ids_for_export.add_variable ids toplevel_my_region in
    let ids = Ids_for_export.add_variable ids toplevel_my_ghost_region in
    let ids = Ids_for_export.add_variable ids toplevel_my_alloc_region in
    Ids_for_export.add_symbol ids module_symbol

  let apply_renaming
      { return_continuation;
        exn_continuation;
        toplevel_my_region;
        toplevel_my_ghost_region;
        toplevel_my_alloc_region;
        module_symbol;
        used_value_slots
      } renaming =
    { return_continuation =
        Renaming.apply_continuation renaming return_continuation;
      exn_continuation = Renaming.apply_continuation renaming exn_continuation;
      toplevel_my_region = Renaming.apply_variable renaming toplevel_my_region;
      toplevel_my_ghost_region =
        Renaming.apply_variable renaming toplevel_my_ghost_region;
      toplevel_my_alloc_region =
        Renaming.apply_variable renaming toplevel_my_alloc_region;
      module_symbol = Renaming.apply_symbol renaming module_symbol;
      used_value_slots
    }
end

type t =
  { body : Flambda.Expr.t;
    metadata : Metadata.t
  }

let create ~return_continuation ~exn_continuation ~toplevel_my_region
    ~toplevel_my_ghost_region ~toplevel_my_alloc_region ~body ~module_symbol
    ~used_value_slots =
  { body;
    metadata =
      { return_continuation;
        exn_continuation;
        toplevel_my_region;
        toplevel_my_ghost_region;
        toplevel_my_alloc_region;
        module_symbol;
        used_value_slots
      }
  }

let create_of_metadata_and_body metadata body = { body; metadata }

let metadata t = t.metadata

let return_continuation t = t.metadata.return_continuation

let exn_continuation t = t.metadata.exn_continuation

let toplevel_my_region t = t.metadata.toplevel_my_region

let toplevel_my_ghost_region t = t.metadata.toplevel_my_ghost_region

let toplevel_my_alloc_region t = t.metadata.toplevel_my_alloc_region

let body t = t.body

let module_symbol t = t.metadata.module_symbol

let used_value_slots t = t.metadata.used_value_slots

let with_used_value_slots t used_value_slots =
  { t with
    metadata = { t.metadata with used_value_slots = Known used_value_slots }
  }

let with_body t body = { t with body }

let [@ocamlformat "disable"] print ppf
      { body; metadata = { return_continuation; exn_continuation;
        toplevel_my_region; toplevel_my_ghost_region;
        toplevel_my_alloc_region; module_symbol; used_value_slots;
      } } =
  Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(module_symbol@ %a)@]@ \
        @[<hov 1>(return_continuation@ %a)@]@ \
        @[<hov 1>(exn_continuation@ %a)@]@ \
        @[<hov 1>(toplevel_my_region@ %a)@]@ \
        @[<hov 1>(toplevel_my_ghost_region@ %a)@]@ \
        @[<hov 1>(toplevel_my_alloc_region@ %a)@]@ \
        @[<hov 1>(used_value_slots@ %a)@]@ \
        @[<hov 1>%a@]\
      )@]"
    Symbol.print module_symbol
    Continuation.print return_continuation
    Continuation.print exn_continuation
    Variable.print toplevel_my_region
    Variable.print toplevel_my_ghost_region
    Variable.print toplevel_my_alloc_region
    (Or_unknown.print Value_slot.Set.print) used_value_slots
    Flambda.Expr.print body
