module Facts_cache = File_cache.Make (struct
  type t = Module_implementation_facts.t option

  let decode module_facts =
    match
      Module_facts_compact.to_facts
        (Index_format.module_facts_block module_facts)
    with
    | Ok facts -> facts
    | Error message -> raise (Module_facts_compact.Malformed message)

  let read file =
    match Index_format.read ~file with
    | Index index -> Option.map decode index.module_facts
    | Cmt _ | Cms _ | Unknown -> raise (Index_format.Not_an_index file)

  let cache_name = "Module_facts_reader"
end)

let fold ~index_files ~init ~f =
  List.fold_left
    (fun accumulator path ->
      match (accumulator, Facts_cache.read path) with
      | None, _ | _, None -> None
      | Some accumulator, Some facts -> Some (f accumulator ~path facts))
    (Some init) index_files

let flush ?older_than () = Facts_cache.flush ?older_than ()
