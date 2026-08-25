(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

include Int_ids.Code_id_or_name

let[@inline always] pattern_match' t ~code_id ~name =
  pattern_match t ~code_id
    ~var:(fun var -> (name [@inlined hint]) (Name.var var))
    ~symbol:(fun symbol -> (name [@inlined hint]) (Name.symbol symbol))

let compilation_unit t =
  pattern_match t ~code_id:Code_id.get_compilation_unit
    ~var:Variable.compilation_unit ~symbol:Symbol.compilation_unit
