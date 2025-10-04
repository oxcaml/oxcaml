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

  let link_partial output_name files_to_link =
    js_of_oxcaml
      (List.concat
         [ ["link"];
           ["-o"; output_name];
           (if !Clflags.link_everything then ["--linkall"] else []);
           (if !Clflags.debug then ["--debug-info"] else []);
           files_to_link;
           List.rev !Clflags.all_ccopts ])

  let link objfiles output_name ~cached_genfns_imports:_ ~genfns:_
      ~units_tolink:_ ~ppf_dump:_ : unit =
    let objfiles =
      ListLabels.map objfiles ~f:(fun obj_name ->
          try Load_path.find obj_name
          with Not_found -> raise (Linkenv.Error (File_not_found obj_name)))
    in
    (* Build the runtime *)
    let runtime = output_name ^ ".runtime.js" in
    (* Extract runtime files from ccobjs *)
    let runtime_files, other_objs =
      ListLabels.partition objfiles ~f:(fun f -> Filename.check_suffix f ".js")
    in
    (* Always build runtime - it's required for JavaScript execution *)
    js_of_oxcaml
      (List.concat
         [ ["build-runtime"];
           ["-o"; runtime];
           (if !Clflags.debug then ["--debug-info"] else []);
           runtime_files ]);
    (* Link everything together *)
    let files_to_link = (runtime :: other_objs) @ objfiles in
    Misc.try_finally
      ~always:(fun () -> Misc.remove_file runtime)
      (fun () -> link_partial output_name files_to_link)

  let create_archive = link_partial

  let link_shared objfiles output_name ~genfns:_ ~units_tolink:_ ~ppf_dump:_ :
      unit =
    link_partial output_name objfiles

  let emit = None

  let compile_implementation (i : Compile_common.info) ~keep_symbol_tables:_
      program =
    let ({ program; imported_compilation_units }
          : Jsoo_imports.Js_backend.program) =
      Flambda2.lambda_to_jsir
        ~machine_width:Target_system.Machine_width.Thirty_two_no_gc_tag_bit
        ~ppf_dump:i.ppf_dump
        ~prefixname:(Unit_info.prefix i.target)
        program
      |> Misc.print_if i.ppf_dump Clflags.dump_jsir
           (fun ppf (jsir : Jsoo_imports.Js_backend.program) ->
             Jsoo_imports.Jsir.Print.program ppf (fun _ _ -> "") jsir.program)
    in
    let output_filename =
      Unit_info.Artifact.filename
        (Unit_info.artifact i.target ~extension:ext_obj)
    in
    let cmj = Unit_info.cmj i.target in
    let cmj_filename = Unit_info.Artifact.filename cmj in
    let open Jsoo_imports in
    let info : Jsoo_imports.Unit_info.t =
      { provides =
          StringSet.singleton
            (Compilation_unit.full_path_as_string i.module_name);
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
    Jsoo_imports.Jsir.save compilation_unit ~filename:cmj_filename;
    Misc.try_finally
      (fun () ->
        js_of_oxcaml
          (List.concat
             [ ["compile"];
               (if !Clflags.debug then ["--debug-info"] else []);
               (* CR jvanburen: re-add jsopts??? *)
               (* !Clflags.all_jsopts; *)
               ["-o"; output_filename];
               [cmj_filename] ]))
      ~always:(fun () -> Misc.remove_file cmj_filename)
end)
