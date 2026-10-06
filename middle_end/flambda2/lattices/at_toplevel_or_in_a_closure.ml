(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2026--2026 OCamlPro SAS                                    *)
(*   Copyright 2026--2026 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type ('toplevel, 'in_a_closure) t =
  | At_toplevel of 'toplevel
  | In_a_closure of 'in_a_closure

let[@ocamlformat "disable"] print ~toplevel ~in_a_closure ppf = function
  | At_toplevel x ->
    Format.fprintf ppf "(at_toplevel@ %a)" toplevel x
  | In_a_closure y ->
      Format.fprintf ppf "(in_a_closure@ %a)" in_a_closure y

let map ~toplevel ~in_a_closure = function
  | At_toplevel x -> At_toplevel (toplevel x)
  | In_a_closure y -> In_a_closure (in_a_closure y)
