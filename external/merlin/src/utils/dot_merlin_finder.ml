let file_exists path = Sys.file_exists path && not (Sys.is_directory path)

let split_path path =
  let dir = Filename.dirname path in
  let abs_dir = Unix.realpath dir in
  let file_rel_to_dir = Filename.basename path in
  let rec loop ~abs_dir ~file_rel_to_dir =
    if file_exists (Filename.concat abs_dir "dune-workspace") then
      Some (~workspace_root:abs_dir, ~rel_path:file_rel_to_dir)
    else
      let parent = Filename.dirname abs_dir in
      if String.equal parent abs_dir then
        (* We've reached the root directory. *)
        None
      else
        loop ~abs_dir:parent
          ~file_rel_to_dir:
            (Filename.concat (Filename.basename abs_dir) file_rel_to_dir)
  in
  loop ~abs_dir ~file_rel_to_dir

let find_in_src_or_build_dir src_path =
  match split_path src_path with
  | Some (~workspace_root, ~rel_path) ->
    let build_path =
      Filename.concat workspace_root (Filename.concat "_build/default" rel_path)
    in
    if file_exists build_path then Some build_path
    else if file_exists src_path then Some src_path
    else None
  | None -> None

let exists_in_src_or_build_dir dir =
  find_in_src_or_build_dir dir |> Option.is_some
