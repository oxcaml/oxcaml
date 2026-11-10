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

(* Under the structured scheme a data symbol is named by a mangling path ending
   in a marker, which tells it apart from the code symbols (whose paths end in
   their stamps) and from source identifiers, followed by its stamp. There is no
   suffix for [shorten_symbol_names] to shorten, as for code IDs. *)
let manufacture_with_path cu name ~structured_path =
  incr manufacture_counter;
  let stamp = !manufacture_counter in
  match Compilation_unit.name_mangling_scheme_for_current_unit () with
  | Flat ->
    let suffix = string_of_int stamp in
    let name =
      if Flambda_features.Expert.shorten_symbol_names ()
      then "s" ^ suffix
      else name ^ "_" ^ suffix
    in
    create cu (Linkage_name.of_string name)
  | Structured ->
    let path = Lazy.force structured_path @ [Structured_mangling.Stamp stamp] in
    Flambda2_import.Symbol.for_structured_mangling_path ~compilation_unit:cu
      ~path
    |> create_wrapped

let data_marker = Structured_mangling.marker_payload "data"

let closure_marker = Structured_mangling.marker_payload "closure"

let manufacture cu name =
  manufacture_with_path cu name
    ~structured_path:
      (lazy Structured_mangling.[Function name; Function data_marker])

let manufacture_for_variable cu var =
  manufacture_with_path cu
    (Variable.canonical_name var)
    ~structured_path:
      (lazy
        Structured_mangling.
          [Function (Variable.raw_name var); Function data_marker])

let manufacture_for_closure cu ~name ~function_slot ~code_id =
  let structured_path =
    lazy
      (let path =
         match code_id with
         | Some code_id -> Code_id.mangling_path code_id
         | None ->
           [Structured_mangling.Function (Function_slot.name function_slot)]
       in
       path @ [Structured_mangling.Function closure_marker])
  in
  manufacture_with_path cu name ~structured_path
