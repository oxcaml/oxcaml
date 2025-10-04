module Make (Flambda2 : Optcomp_intf.Flambda2) = Optcompile.Make (struct
  let backend = Compile_common.Js_of_ocaml

  let ext_obj = ".cmjo"

  let ext_lib = ".cmja"

  let ext_flambda_obj = ".cmjx"

  let ext_flambda_lib = ".cmjxa"

  let default_executable_name = Config.default_executable_name ^ ".js"

  let js_of_oxcaml args =
    let prog =
      (* Use jsoo from our PATH when we're bootstrapping *)
      match Sys.ocaml_release with
      | { extra = Some (Plus, "ox"); _ } ->
        Filename.concat Config.bindir "js_of_oxcaml"
      | _ ->
        (* Try to find js_of_oxcaml in the same directory as the current executable *)
        let exe_dir = Filename.dirname Sys.executable_name in
        let jsoo_path = Filename.concat exe_dir "js_of_oxcaml" in
        if Sys.file_exists jsoo_path then jsoo_path else "js_of_oxcaml"
    in
    let cmdline = Filename.quote_command prog args in
    match Ccomp.command cmdline with 0 -> () | _ -> raise (Sys_error cmdline)

  let link_args ?(linkall = false) ?(include_jsopts = true) subcommand
      output_name extra_args files =
    let debug_flag = if !Clflags.debug then ["--debug-info"] else [] in
    let linkall_flag =
      if linkall && !Clflags.link_everything then ["--linkall"] else []
    in
    let jsopts_args =
      if include_jsopts then List.rev !Clflags.all_jsopts else []
    in
    [subcommand; "-o"; output_name]
    @ linkall_flag @ debug_flag @ files @ extra_args @ jsopts_args

  let find_files files =
    ListLabels.map files ~f:(fun name ->
        try Load_path.find name
        with Not_found -> raise (Linkenv.Error (File_not_found name)))

  let link_partial output_name files_to_link =
    js_of_oxcaml (link_args ~linkall:true "link" output_name [] files_to_link)

  let link objfiles output_name ~cached_genfns_imports:_ ~genfns:_
      ~units_tolink:_ ~ppf_dump:_ : unit =
    let objfiles = find_files objfiles in
    let js_support_files = find_files !Clflags.js_stubs in
    let runtime_files, unexpected =
      List.partition (fun f -> Filename.check_suffix f ".js") js_support_files
    in
    Clflags.js_stubs := runtime_files;
    if !Clflags.verbose
    then
      List.iter
        (fun file ->
          Format.eprintf
            "ocamlopt: ignoring non-JavaScript support file %s when targeting \
             js_of_ocaml@."
            file)
        unexpected;
    let runtime = output_name ^ ".runtime.js" in
    js_of_oxcaml
      (link_args ~linkall:false ~include_jsopts:false "build-runtime" runtime []
         runtime_files);
    let files_to_link = runtime :: objfiles in
    Misc.try_finally
      ~always:(fun () -> Misc.remove_file runtime)
      (fun () -> link_partial output_name files_to_link)

  let create_archive output_name files =
    let files =
      ListLabels.map files ~f:(fun obj_name ->
          try Load_path.find obj_name
          with Not_found -> raise (Linkenv.Error (File_not_found obj_name)))
    in
    js_of_oxcaml (link_args ~linkall:true "link" output_name ["-a"] files)

  let link_shared objfiles output_name ~genfns:_ ~units_tolink:_ ~ppf_dump:_ :
      unit =
    let objfiles =
      ListLabels.map objfiles ~f:(fun obj_name ->
          try Load_path.find obj_name
          with Not_found -> raise (Linkenv.Error (File_not_found obj_name)))
    in
    link_partial output_name objfiles

  let emit = None

  let compile_implementation ~keep_symbol_tables:_ ~sourcefile:_ ~prefixname
      ~ppf_dump program =
    let for_pack_prefix = Compilation_unit.Prefix.from_clflags () in
    let module_name =
      Unit_info.Artifact.from_filename ~for_pack_prefix
        (prefixname ^ ext_flambda_obj)
      |> Unit_info.Artifact.modname
    in
    let open Jsoo_imports in
    let ({ program; imported_compilation_units } : Js_backend.program) =
      Targetint.set_num_bits 32;
      Flambda2.lambda_to_jsir ~machine_width:Thirty_two_no_gc_tag_bit ~ppf_dump
        ~prefixname program
      |> Misc.print_if ppf_dump Clflags.dump_jsir
           (fun ppf (jsir : Js_backend.program) ->
             Jsir.Print.program ppf (fun _ _ -> "") jsir.program)
    in
    let output_filename = prefixname ^ ext_obj in
    let jsir_filename = prefixname ^ ".jsir" in
    let info : Unit_info.t =
      { provides =
          StringSet.singleton (Compilation_unit.full_path_as_string module_name);
        requires =
          Compilation_unit.Set.elements imported_compilation_units
          |> ListLabels.map ~f:Compilation_unit.full_path_as_string
          |> StringSet.of_list;
        primitives = [];
        aliases = [];
        force_link = !Clflags.link_everything;
        effects_without_cps = false
      }
    in
    let compilation_unit : Jsir.compilation_unit =
      { info;
        contents =
          { code = program;
            cmis = StringSet.empty;
            debug =
              Jsir.Debug.default_summary (* CR jvanburen: add real debuginfo *)
          }
      }
    in
    Jsir.save compilation_unit ~filename:jsir_filename;
    Misc.try_finally
      (fun () ->
        js_of_oxcaml
          (List.concat
             [ ["compile"];
               (if !Clflags.debug then ["--debug-info"] else []);
               ["-o"; output_filename];
               [jsir_filename];
               List.rev !Clflags.all_jsopts ]))
      ~always:(fun () -> Misc.remove_file jsir_filename)
end)
