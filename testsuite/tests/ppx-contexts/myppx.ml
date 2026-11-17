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
         (* Note: we do not test include_dirs, load_path
            as they produce non-portable paths *)
      Printf.eprintf "include_dirs: [%s]\n"
        (quote_strings !Clflags.include_dirs);
      Printf.eprintf "load_path: [%s]\n"
        (quote_strings !Config.load_path);
      *)
      let quote_scope_arg : Clflags.scope_arg -> string = function
        | Open s -> Printf.sprintf "Open(%S)" s
        | Open_cmi s -> Printf.sprintf "Open_cmi(%S)" s
        | Open_h s -> Printf.sprintf "Open_h(%S)" s
        | Expose (intf, name) -> Printf.sprintf "Expose(%S, %S)" intf name
      in
      Printf.eprintf "scope_args: [%s]\n"
        (List.map quote_scope_arg !Clflags.scope_args |> String.concat " ");
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
