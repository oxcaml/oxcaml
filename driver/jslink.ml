
let link ~ppf_dump:(_ : Format.formatter) objfiles output_name =
  Profile.(record_call (annotate_file_name output_name)) (fun () ->
      let stdlib = Filename.concat Config.standard_library "stdlib.cmja" in
      let stdexit = Filename.concat Config.standard_library "std_exit.cmj" in
      let objfiles, runtime_files =
        ListLabels.partition_map objfiles ~f:(fun file ->
          if Filename.check_suffix file ".cmj.js"
          || Filename.check_suffix file ".cmja.js"
          then Either.Left file
          else if Filename.check_suffix file ".js"
          then Either.Right file
          else Misc.fatal_errorf "Cannot link, not a javascript file: %s" file)
      in
      let runtime =
        output_name ^ ".runtime.js"
      in
      Jscompile.run_jsoo_exn ~args:([ "build-runtime"; "-o"; runtime ] @ runtime_files);
      Misc.try_finally
        (fun () ->
          let files =
            if !Clflags.nopervasives then runtime :: objfiles
            else runtime :: stdlib :: (objfiles @ [stdexit])
          in
          Jscompile.run_jsoo_exn
            ~args:(["link"; "-o"; output_name ] @ files))
        ~always:(fun () -> Misc.remove_file output_name))
