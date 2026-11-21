(* A simple PPX *)

open Ast_mapper

let () =
  let quote_option = function
    | None -> "None"
    | Some s -> Printf.sprintf "Some(%S)" s in
  register "test" (fun _ ->
      Printf.eprintf "<ppx-context>\n";
      Printf.eprintf "tool_name: %S\n"
        (tool_name ());
      (*
         (* Note: we do not test include_dirs, hidden_include_dirs, load_path
         as they produce non-portable paths. *)
      let quote_dirs dirs =
        List.map
          (fun (d : Clflags.include_dir) ->
            Printf.sprintf "%S%s" d.path
              (if d.cmx_guaranteed then "(x)" else ""))
          dirs
        |> String.concat " "
      in
      Printf.eprintf "include_dirs: [%s]\n" (quote_dirs !Clflags.include_dirs);
      Printf.eprintf "hidden_include_dirs: [%s]\n"
        (quote_dirs !Clflags.hidden_include_dirs);
      let Load_path.{ visible; hidden } = Load_path.get_paths () in
      Printf.eprintf "load_path.visible: [%s]\n" (quote_dirs visible);
      Printf.eprintf "load_path.hidden: [%s]\n" (quote_dirs hidden);
      *)
      let quote_open_arg : Clflags.open_arg -> string = function
        | Open s -> Printf.sprintf "Open(%S)" s
        | Open_cmi { path; cmx_guaranteed = false } ->
          Printf.sprintf "Open_cmi(%S)" path
        | Open_cmi { path; cmx_guaranteed = true } ->
          Printf.sprintf "Open_cmi_x(%S)" path
      in
      Printf.eprintf "open_args: [%s]\n"
        (List.map quote_open_arg !Clflags.open_args |> String.concat " ");
      Printf.eprintf "for_package: %S\n"
        (quote_option !Clflags.for_package);
      Printf.eprintf "use_debug: %B\n"
        !Clflags.debug;
      Printf.eprintf "use_threads: %B\n"
        !Clflags.use_threads;
      Printf.eprintf "recursive_types: %B\n"
        !Clflags.recursive_types;
      Printf.eprintf "principal: %B\n"
        !Clflags.principal;
      Printf.eprintf "no_alias_deps: %B\n"
        !Clflags.no_alias_deps;
      Printf.eprintf "unboxed_types: %B\n"
        !Clflags.unboxed_types;
      Printf.eprintf "</ppx-context>\n";
      flush stderr;
      default_mapper);
