
let link ~ppf_dump:(_ : Format.formatter) objfiles output_name =
  Profile.(record_call (annotate_file_name output_name)) (fun () ->
    let jsoo = Filename.concat Config.bindir "js_of_ocaml" in
    let files =
      let runtime = Filename.concat Config.standard_library "runtime.js" in
      let stdlib = Filename.concat Config.standard_library "stdlib.js" in
      let stdexit = Filename.concat Config.standard_library "std_exit.js" in
      let objfiles =
      ListLabels.map objfiles ~f:(fun file ->
        match Filename.extension file with
         | ".js" -> file
         | ".cmja" | ".cmj" -> file ^ ".js"
         | ".cmjx" -> Filename.remove_extension file ^ ".cmj.js"
         | _ -> Misc.fatal_errorf "Cannot link, not a javascript file: %s" file)
      in
      if !Clflags.nopervasives then runtime :: objfiles
      else runtime :: stdlib :: (objfiles @ [stdexit])
    in
    Ccomp.run_command
      (Filename.quote_command jsoo (["link"; "-o"; output_name ] @ files)))
