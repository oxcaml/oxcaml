(* Downstream: generates Ocaml_shadow, as ocaml-compiler-libs did: a deprecated
   alias for each module whose interface is in the given compiler-libs
   directories. *)

let () =
  let dirs, out =
    match Array.to_list Sys.argv with
    | _ :: "-o" :: out :: dirs -> (dirs, out)
    | _ -> failwith "usage: gen.exe -o FILE DIR..."
  in
  let modules =
    List.concat_map
      (fun dir ->
        Sys.readdir dir |> Array.to_list
        |> List.filter (fun f -> Filename.check_suffix f ".cmi")
        |> List.map (fun f ->
               String.capitalize_ascii (Filename.chop_extension f)))
      dirs
    |> List.sort_uniq String.compare
  in
  let oc = open_out out in
  output_string oc "module Do_not_use_directly = struct end\n";
  List.iter
    (fun m ->
      Printf.fprintf oc
        "module %s = Do_not_use_directly\n\
        \  [@@deprecated \"Accessing this module directly is deprecated\"]\n"
        m)
    modules;
  close_out oc
