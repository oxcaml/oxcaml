let link ~ppf_dump:(_ : Format.formatter) objfiles output_name =
  Profile.(record_call (annotate_file_name output_name)) (fun () ->
      let stdlib = Filename.concat Config.standard_library "stdlib.cmja" in
      let stdexit = Filename.concat Config.standard_library "std_exit.cmjo" in
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
      Jscompile.run_jsoo_exn ~args:([ "build-runtime"; "-o"; runtime ] @ runtime_files);
      Misc.try_finally
        (fun () ->
          (* Prepare files to link, including stdlib if needed *)
          let files =
            if !Clflags.nopervasives then runtime :: objfiles
            else runtime :: stdlib :: (objfiles @ [stdexit])
          in
          (* Link everything together *)
          Jscompile.run_jsoo_exn
            ~args:(["link"; "-o"; output_name ] @ files))
        ~always:(fun () -> Misc.remove_file runtime))