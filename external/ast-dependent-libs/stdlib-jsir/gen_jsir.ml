(* Generates the dune rules compiling the standard library and the otherlibs
   for ocamlopt -target js_of_ocaml.

   The libraries are rebuilt from the sources in the tree, against the .cmi
   files of the installed compiler, so that the JSIR artifacts are
   interchangeable with the native ones. Which units a library has, in which
   order they link, what they import and whether the archive is -linkall are
   read off the installed native archive with ocamlobjinfo rather than
   rederived (see library.ml); the flags come from the flags.sexp the
   library's own dune file includes (see spec.ml).

     gen_jsir -ocaml-where DIR -ocamlobjinfo EXE -install FILE
              [FLAGS.sexp...] LIB...

   writes the rules of all the libraries, in one directory, to stdout, and
   the list of files to install to FILE. *)

let usage =
  "gen_jsir -ocaml-where DIR -ocamlobjinfo EXE -install FILE [FLAGS.sexp...] \
   LIB..."

let ocamlobjinfo exe = function
  | [] -> []
  | files ->
    let tmp = Filename.temp_file "gen_jsir" ".objinfo" in
    let cmd =
      Filename.quote_command exe ~stdout:tmp
        ("-no-approx" :: "-no-code" :: files)
    in
    if Sys.command cmd <> 0 then failwith ("failed: " ^ cmd);
    Fun.protect
      ~finally:(fun () -> Sys.remove tmp)
      (fun () -> Objinfo.parse_file tmp)

let () =
  let ocaml_where = ref None and objinfo = ref None and install = ref None in
  let libs = ref [] and flags_files = ref [] in
  Arg.parse
    [ "-ocaml-where", Arg.String (fun d -> ocaml_where := Some d),
      "DIR the installed compiler's library directory";
      "-ocamlobjinfo", Arg.String (fun e -> objinfo := Some e),
      "EXE the ocamlobjinfo to run on the installed archives";
      "-install", Arg.String (fun f -> install := Some f),
      "FILE where to write the list of files to install" ]
    (fun a ->
      if Filename.check_suffix a ".sexp" then flags_files := a :: !flags_files
      else libs := a :: !libs)
    usage;
  let ocaml_where, objinfo, install =
    match !ocaml_where, !objinfo, !install with
    | Some d, Some e, Some f -> d, e, f
    | _ -> prerr_endline usage; exit 2
  in
  (* otherlibs/LIB/flags.sexp *)
  let flags_file lib =
    List.find_opt
      (fun f -> Filename.(basename (dirname f)) = lib)
      !flags_files
  in
  (* In the order given, which must list a library after those it requires. *)
  let library done_ lib =
    let spec = Spec.of_name lib in
    let expand = Spec.expand ~ocaml_where in
    let archive = ocamlobjinfo objinfo [ expand spec.archive ] in
    let extra = ocamlobjinfo objinfo (List.map expand spec.extra) in
    let requires =
      List.map
        (fun r ->
          match List.assoc_opt r done_ with
          | Some (_, required) -> required
          | None -> failwith (lib ^ " must be listed after " ^ r))
        spec.requires
    in
    let library =
      Library.create ~name:lib ~source:(Spec.source spec ~ocaml_where)
        ~installed_dir:spec.installed_dir ~requires
        ~flags:(Spec.flags spec ~flags_file:(flags_file lib))
        ~unit_flags:spec.unit_flags ~archive ~extra
    in
    (lib, (spec, library)) :: done_
  in
  let libs = List.fold_left library [] (List.rev !libs) |> List.rev_map snd in
  Emit.libraries stdout libs;
  Out_channel.with_open_text install (fun oc ->
    Emit.install oc ~build_dir:"build" libs)
