(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

include Slot.Make (struct
  let colour = Flambda_colours.value_slot

  type payload = Flambda_kind.t * is_always_immediate:bool

  let print_payload ppf (kind, ~is_always_immediate) =
    Format.fprintf ppf " @<1>\u{2237} %a%s" Flambda_kind.print kind
      (if is_always_immediate then "(immediate)" else "")
end)

let create compilation_unit ~name ~is_always_immediate kind =
  create compilation_unit ~name (kind, ~is_always_immediate)

let kind t =
  let kind, ~is_always_immediate:_ = payload t in
  kind

let is_always_immediate t =
  let _, ~is_always_immediate = payload t in
  is_always_immediate
