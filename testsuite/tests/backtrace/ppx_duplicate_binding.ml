(* A ppx which, for every value binding marked with [@duplicate], adds a copy
   of the binding whose name has "_copy" appended.  Everything else about the
   copy, in particular every location it contains, is left unchanged, as it is
   for the several specialisations of one function that a ppx generates. *)

open Parsetree
open Ast_mapper

let is_duplicate attr = String.equal attr.attr_name.txt "duplicate"

let has_duplicate vb = List.exists is_duplicate vb.pvb_attributes

let remove_duplicate vb =
  { vb with
    pvb_attributes =
      List.filter (fun attr -> not (is_duplicate attr)) vb.pvb_attributes }

let rename_copy vb =
  let vb = remove_duplicate vb in
  match vb.pvb_pat.ppat_desc with
  | Ppat_var name ->
    let name = { name with txt = name.txt ^ "_copy" } in
    { vb with pvb_pat = { vb.pvb_pat with ppat_desc = Ppat_var name } }
  | _ -> failwith "[@duplicate] expects a variable pattern"

let duplicate mapper item =
  let item = default_mapper.structure_item mapper item in
  match item.pstr_desc with
  | Pstr_value (rec_flag, bindings) when List.exists has_duplicate bindings ->
    [ { item with
        pstr_desc = Pstr_value (rec_flag, List.map remove_duplicate bindings) };
      { item with
        pstr_desc = Pstr_value (rec_flag, List.map rename_copy bindings) } ]
  | _ -> [item]

let () =
  register "duplicate_binding" (fun _ ->
    { default_mapper with
      structure = (fun mapper items ->
        List.concat_map (duplicate mapper) items) })
