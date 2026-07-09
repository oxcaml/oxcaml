(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

include Int_ids.Symbol

let is_predefined_exception t =
  Compilation_unit.equal (compilation_unit t) Compilation_unit.predef_exn

let manufacture =
  let c = ref (-1) in
  fun cu name ->
    let suffix =
      incr c;
      "_" ^ string_of_int !c
    in
    let name =
      if Flambda_features.Expert.shorten_symbol_names ()
      then "s" ^ suffix
      else name ^ suffix
    in
    create cu (Linkage_name.of_string name)
