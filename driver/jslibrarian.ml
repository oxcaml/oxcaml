type error = File_not_found of string | Not_an_object_file of string

exception Error of error

let create_archive file_list lib_name =
  (* Collect .cmjo files that will be linked into the archive *)
  let cmjo_files =
    List.map (fun name ->
      let file_name =
        try Load_path.find name
        with Not_found -> raise (Error (File_not_found name))
      in
      (* Check if it's a .cmjo file *)
      if Filename.check_suffix file_name ".cmjo" then
        file_name
      else if Filename.check_suffix file_name ".cmja" then
        (* .cmja files are already JavaScript archives, just include them *)
        file_name
      else
        raise (Error (Not_an_object_file file_name))
    ) file_list
  in
  (* Use js_of_ocaml link to create the archive *)
  (* The -a flag is crucial - it makes js_of_ocaml preserve unitInfo metadata *)
  let linkall_flag = if !Clflags.link_everything then ["--linkall"] else [] in
  let debug_flag = if !Clflags.debug then ["--debug-info"] else [] in
  Jscompile.run_jsoo_exn
    ~args:([ "link"; "-a" ] @ linkall_flag @ debug_flag @ [ "-o"; lib_name ] @ cmjo_files)

open Format
module Style = Misc.Style

let report_error ppf = function
  | File_not_found name ->
      fprintf ppf "Cannot find file %a" Style.inline_code name
  | Not_an_object_file name ->
      fprintf ppf "The file %a is not a JavaScript object file (.cmjo) or archive (.cmja)"
        (Style.as_inline_code Location.print_filename)
        name

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)

let reset () = ()