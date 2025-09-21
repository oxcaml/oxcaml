let link ~ppf_dump:(_ : Format.formatter) objfiles output_name =
  Profile.(record_call (annotate_file_name output_name)) (fun () ->
      let stdlib = "stdlib.cmja" in
      let stdexit = "std_exit.cmjo" in
      (* Add stdlib to objfiles unless -nopervasives is set *)
      let objfiles =
        if !Clflags.nopervasives then objfiles
        else stdlib :: objfiles @ [stdexit]
      in
      (* Partition input files into JavaScript object files and runtime files *)
      let objfiles, runtime_files =
        ListLabels.partition_map objfiles ~f:(fun file ->
          if Filename.check_suffix file ".cmjo"
          || Filename.check_suffix file ".cmja"
          then Either.Left file
          else if Filename.check_suffix file ".js"
          then Either.Right file
          else Misc.fatal_errorf "Cannot link, not a javascript file: %s" file)
      in
      (* Build the runtime *)
      let runtime = output_name ^ ".runtime.js" in
      Jscompile.run_jsoo_exn ~args:([ "build-runtime"; "--enable=effects,with-js-error"; "-o"; runtime ] @ runtime_files);
      Misc.try_finally
        (fun () ->
          let find_file name =
            try Load_path.find name
            with Not_found ->
              failwith (Printf.sprintf "Cannot find %s in include directories" name)
          in
          let files = runtime :: List.map find_file objfiles in
          let debug_flag = if !Clflags.debug then ["--debug-info"] else [] in
          let linkall_flag = if !Clflags.link_everything then ["--linkall"] else [] in
          Jscompile.run_jsoo_exn
            ~args:(["link"; "-o"; output_name ] @ linkall_flag @ debug_flag @ files))
        ~always:(fun () -> Misc.remove_file runtime))
