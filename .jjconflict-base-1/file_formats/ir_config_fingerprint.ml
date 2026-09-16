(* The configuration items recorded here are the ones that (a) influence code
   generation decisions baked into saved IR and (b) are consulted again when
   the IR is reloaded and compilation resumes, e.g. by Emit to compute frame
   sizes and stack-slot offsets. If the saving and reloading processes
   disagree on any of them, the reloaded IR would be silently miscompiled. *)

type entry =
  { name : string;
    value : string;
  }

type t = entry list

let current () =
  [ { name = "with_frame_pointers";
      value = string_of_bool Config.with_frame_pointers };
    { name = "with_address_sanitizer";
      value = string_of_bool Config.with_address_sanitizer };
    { name = "omit_leaf_frame_pointers";
      value = string_of_bool !Oxcaml_flags.omit_leaf_frame_pointers } ]

type mismatch =
  { name : string;
    saved_value : string;
    current_value : string;
  }

let mismatches ~saved ~current =
  let names =
    List.sort_uniq String.compare
      (List.map (fun (entry : entry) -> entry.name) (saved @ current))
  in
  List.filter_map
    (fun name ->
      let value_or_absent fingerprint =
        match
          List.find_opt (fun (entry : entry) -> String.equal entry.name name)
            fingerprint
        with
        | Some { name = _; value } -> value
        | None -> "<absent>"
      in
      let saved_value = value_or_absent saved in
      let current_value = value_or_absent current in
      if String.equal saved_value current_value
      then None
      else Some { name; saved_value; current_value })
    names

type configuration_mismatch =
  { filename : string;
    entries : mismatch list;
  }

let read_and_check ic ~filename ~raise_configuration_mismatch =
  let saved = (input_value ic : t) in
  match mismatches ~saved ~current:(current ()) with
  | [] -> ()
  | entries -> raise_configuration_mismatch { filename; entries }

let print_configuration_mismatch ppf { filename; entries } =
  Format_doc.fprintf ppf
    "%a@ was saved with a configuration incompatible with the current \
     process:%a"
    Location.Doc.quoted_filename filename
    (fun ppf entries ->
       List.iter
         (fun { name; saved_value; current_value } ->
            Format_doc.fprintf ppf "@ %s: saved %s, current %s" name
              saved_value current_value)
         entries)
    entries
