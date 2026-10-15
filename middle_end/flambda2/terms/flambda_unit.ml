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
      toplevel_my_alloc_region : Variable.t;
      module_symbol : Symbol.t
    }

  let module_symbol t = t.module_symbol

  let ids_for_export
      { return_continuation;
        exn_continuation;
        toplevel_my_alloc_region;
        module_symbol
      } =
    let ids = Ids_for_export.empty in
    let ids = Ids_for_export.add_continuation ids return_continuation in
    let ids = Ids_for_export.add_continuation ids exn_continuation in
    let ids = Ids_for_export.add_variable ids toplevel_my_alloc_region in
    Ids_for_export.add_symbol ids module_symbol

  let apply_renaming
      { return_continuation;
        exn_continuation;
        toplevel_my_alloc_region;
        module_symbol
      } renaming =
    { return_continuation =
        Renaming.apply_continuation renaming return_continuation;
      exn_continuation = Renaming.apply_continuation renaming exn_continuation;
      toplevel_my_alloc_region =
        Renaming.apply_variable renaming toplevel_my_alloc_region;
      module_symbol = Renaming.apply_symbol renaming module_symbol
    }

  let print ppf
      { return_continuation;
        exn_continuation;
        toplevel_my_alloc_region;
        module_symbol
      } =
    let open! Misc.Sexp in
    print ppf
      [ a "module_symbol" module_symbol Symbol.print;
        a "return_continuation" return_continuation Continuation.print;
        a "exn_continuation" exn_continuation Continuation.print;
        a "toplevel_my_alloc_region" toplevel_my_alloc_region Variable.print ]
end

type t =
  { body : Flambda.Expr.t;
    metadata : Metadata.t
  }

let create ~return_continuation ~exn_continuation ~toplevel_my_alloc_region
    ~body ~module_symbol =
  { body;
    metadata =
      { return_continuation;
        exn_continuation;
        toplevel_my_alloc_region;
        module_symbol
      }
  }

let create_of_metadata_and_body metadata body = { body; metadata }

let metadata t = t.metadata

let return_continuation t = t.metadata.return_continuation

let exn_continuation t = t.metadata.exn_continuation

let toplevel_my_alloc_region t = t.metadata.toplevel_my_alloc_region

let body t = t.body

let module_symbol t = t.metadata.module_symbol

let with_body t body = { t with body }

let print ppf { body; metadata } =
  let open! Misc.Sexp in
  print ppf
    [a "metadata" metadata Metadata.print; fmt "%a" Flambda.Expr.print body]
