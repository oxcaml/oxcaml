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
    module_symbol : Symbol.t;
    module_block_cells : Symbol.t list
  }

let create ~return_continuation ~exn_continuation ~toplevel_my_alloc_region
    ~body ~module_symbol ~module_block_cells =
  { return_continuation;
    exn_continuation;
    toplevel_my_alloc_region;
    body;
    module_symbol;
    module_block_cells
  }

let return_continuation t = t.return_continuation

let exn_continuation t = t.exn_continuation

let toplevel_my_alloc_region t = t.toplevel_my_alloc_region

let body t = t.body

let module_symbol t = t.module_symbol

let module_block_cells t = t.module_block_cells

let root_symbols t =
  if !Clflags.jsir
  then t.module_symbol :: t.module_block_cells
  else t.module_block_cells

let with_body t body = { t with body }

let [@ocamlformat "disable"] print ppf
      { return_continuation; exn_continuation; toplevel_my_alloc_region; body;
        module_symbol; module_block_cells;
      } =
  Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(module_symbol@ %a)@]@ \
        @[<hov 1>(module_block_cells@ (%a))@]@ \
        @[<hov 1>(return_continuation@ %a)@]@ \
        @[<hov 1>(exn_continuation@ %a)@]@ \
        @[<hov 1>(toplevel_my_alloc_region@ %a)@]@ \
        @[<hov 1>%a@]\
      )@]"
    Symbol.print module_symbol
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Symbol.print)
    module_block_cells
    Continuation.print return_continuation
    Continuation.print exn_continuation
    Variable.print toplevel_my_alloc_region
    Flambda.Expr.print body
