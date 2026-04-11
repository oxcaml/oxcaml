let () =
  let resolve_in_path prog =
    let tmp = Filename.temp_file "ocaml-system" ".path" in
    let command =
      Printf.sprintf "command -v %s > %s" prog (Filename.quote tmp)
    in
    if Sys.command command <> 0
    then failwith ("Could not resolve " ^ prog ^ " from PATH")
    else
      let ic = open_in tmp in
      let resolved = input_line ic in
      close_in ic;
      Sys.remove tmp;
      resolved
  in
  let ocamlc =
    match Sys.getenv_opt "OCAMLC" with
    | Some ocamlc -> ocamlc
    | None -> resolve_in_path "ocamlc"
  in
  if Sys.ocaml_version <> "5.4.0+ox"
  then (
    Printf.eprintf
      "ERROR: The compiler found at %s has version %s,\n\
       and this package requires 5.4.0+ox.\n\
       You should use e.g. 'opam switch create ocaml-system.5.4.0+ox.%s' \
       instead."
      ocamlc
      Sys.ocaml_version
      Sys.ocaml_version;
    exit 1)
  else
    let ocamlc_digest = Digest.to_hex (Digest.file ocamlc) in
    let libdir =
      if Sys.command (ocamlc ^ " -where > ocaml-system.config") = 0
      then
        let ic = open_in "ocaml-system.config" in
        let r = input_line ic in
        close_in ic;
        Sys.remove "ocaml-system.config";
        r
      else failwith "Bad return from 'ocamlc -where'"
    in
    let graphics = Filename.concat libdir "graphics.cmi" in
    let graphics_digest =
      if Sys.file_exists graphics
      then Digest.to_hex (Digest.file graphics)
      else String.make 32 '0'
    in
    let oc = open_out "ocaml-system.config" in
    Printf.fprintf
      oc
      "opam-version: \"2.0\"\n\
       file-depends: [ [ %S %S ] [ %S %S ] ]\n\
       variables { path: %S }\n"
      ocamlc
      ocamlc_digest
      graphics
      graphics_digest
      (Filename.dirname ocamlc);
    close_out oc
