open Owee_buf

type attribute = {
  name : Owee_attribute.t;
  form : Owee_form.t;
  data : u128 option
}

type abbrev = {
  tag : Owee_tag.t;
  has_child : bool;
  attributes : attribute list
}

let read_declaration t =
  let abbrev_code = Read.uleb128 t in
  if abbrev_code = 0 then
    None
  else
    let tag = Owee_tag.of_int_exn (Read.uleb128 t) in
    let has_child = (Read.u8 t) <> 0 in
    let rec read_entry t =
      let name = Read.uleb128 t in
      let form = Read.uleb128 t in
      if form = 0 && name = 0 then
        []
      else
        let form = Owee_form.of_int_exn form in
        let name = Owee_attribute.of_int_exn name in
        let data =
          match form with
          | `implicit_const ->
            Some (Read.sleb128 t)
          | _ -> None
        in
        {name; form; data} :: read_entry t
    in
    let attributes = read_entry t in
    Some (
      abbrev_code, { tag;
                     attributes = attributes;
                     has_child }
    )

let read_one cursor =
  let finalize lst =
    List.fold_left
      (fun acc (code, abbrev) -> U128Map.add code abbrev acc)
      U128Map.empty
      lst
  in
  let rec aux acc =
    match read_declaration cursor with
    | None -> finalize acc
    | Some e -> aux (e :: acc)
  in
  aux []


(* An `abbrevs` entry maps each abbrev code to its list of attributes *)
type abbrevs = abbrev U128Map.t

(* The abbrev section maps binary offset in the section to the corresponding
   `abbrevs` entry. *)
type t = abbrevs S128Map.t

let get ~offset ~code t =
  match S128Map.find_opt offset t with
  | None -> None
  | Some m ->
    U128Map.find_opt code m

let read sections map =
  match Owee_elf.find_section sections ".debug_abbrev" with
  | None -> S128Map.empty
  | Some section ->
    let body = Owee_buf.cursor (Owee_elf.section_body map section) in
    let rec aux acc cursor =
      if at_end cursor then acc
      else
        let offset = cursor.position in
        let abbrevs = read_one cursor in
        let acc = S128Map.add offset abbrevs acc in
        aux acc cursor
    in
    aux S128Map.empty body

let iter ~f t = S128Map.iter (fun offset abbrevs -> f ~offset ~abbrevs) t
