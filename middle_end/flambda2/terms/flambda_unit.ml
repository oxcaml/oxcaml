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
    toplevel_my_alloc_region : Variable.t;
    body : Flambda.Expr.t;
    module_symbol : Symbol.t
  }

let create ~return_continuation ~exn_continuation ~toplevel_my_alloc_region
    ~body ~module_symbol =
  { return_continuation;
    exn_continuation;
    toplevel_my_alloc_region;
    body;
    module_symbol
  }

let return_continuation t = t.return_continuation

let exn_continuation t = t.exn_continuation

let toplevel_my_alloc_region t = t.toplevel_my_alloc_region

let body t = t.body

let module_symbol t = t.module_symbol

let with_body t body = { t with body }

let print ppf
    { return_continuation;
      exn_continuation;
      toplevel_my_alloc_region;
      body;
      module_symbol
    } =
  let open! Misc.Sexp in
  print ppf
    [ a "module_symbol" module_symbol Symbol.print;
      a "return_continuation" return_continuation Continuation.print;
      a "exn_continuation" exn_continuation Continuation.print;
      a "toplevel_my_alloc_region" toplevel_my_alloc_region Variable.print;
      fmt "%a" Flambda.Expr.print body ]
