let link ~ppf_dump:(_ : Format.formatter) objfiles output_name =
  Profile.(record_call (annotate_file_name output_name)) (fun () ->
      let stdlib = "stdlib.cmja" in
      let stdexit = "std_exit.cmjo" in
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
          (* Find stdlib and std_exit in include directories or standard library *)
          let search_dirs = "." :: !Clflags.include_dirs @ [Config.standard_library] in
          let find_file name =
            let rec search = function
              | [] -> failwith (Printf.sprintf "Cannot find %s in include directories" name)
              | dir :: rest ->
                  let path = Filename.concat dir name in
                  if Sys.file_exists path then path else search rest
            in
            if Filename.is_implicit name then search search_dirs else name
          in
          (* Prepare files to link, including stdlib if needed *)
          let files =
            if !Clflags.nopervasives then runtime :: objfiles
            else runtime :: (find_file stdlib) :: (List.map find_file objfiles @ [find_file stdexit])
          in
          (* Link everything together - no -I flags needed for link command *)
          let debug_flag = if !Clflags.debug then ["--debug-info"] else [] in
          Jscompile.run_jsoo_exn
            ~args:(["link"; "-o"; output_name ] @ debug_flag @ files))
        ~always:(fun () -> Misc.remove_file runtime))
