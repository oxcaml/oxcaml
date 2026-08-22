module Facts_cache = File_cache.Make (struct
  type t = Module_implementation_facts.t list

  let read file =
    let index = Index_format.read_exn ~file in
    Index_format.module_facts_list (Option.get index.module_facts)

  let cache_name = "Module_facts_reader"
end)

let fold ~index_files ~init ~f =
  List.fold_left
    (fun accumulator path ->
      List.fold_left
        (fun accumulator facts -> f accumulator ~path facts)
        accumulator (Facts_cache.read path))
    init index_files

let flush ?older_than () = Facts_cache.flush ?older_than ()
