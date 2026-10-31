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

let manufacture_counter = ref (-1)

let manufacture cu name =
  let suffix =
    incr manufacture_counter;
    string_of_int !manufacture_counter
  in
  let name =
    if Flambda_features.Expert.shorten_symbol_names ()
    then "s" ^ suffix
    else name ^ "_" ^ suffix
  in
  create cu (Linkage_name.of_string name)

let export_manufacture_counter () = !manufacture_counter

let restore_manufacture_counter counter =
  if !manufacture_counter = -1
  then manufacture_counter := counter
  else
    Misc.fatal_errorf
      "Restoring symbol manufacture counter would overwrite modified value %d"
      !manufacture_counter
