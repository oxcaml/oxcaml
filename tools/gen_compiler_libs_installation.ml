let target path =
  let basename = Filename.basename path in
  let prefix = Filename.chop_extension basename in
  let extension = Filename.extension basename in
  let tgt_prefix_opt =
    match prefix, extension with
    | "boot_ocamlopt", ".mli"
    | "oxcaml_main", ".mli"
    | "oxcaml_main_native", ".mli"
    | "internal_assembler", ".mli"
    | "mach_checks", ".mli"
    | "relocation_table", ".mli"
    | "symbol_entry", ".mli"
    | "symbol_utils", ".mli"
    | "optmaindriver", ".cmx" ->
      None
    (* parser.pp.ml is parser.ml as compiled (with the menhir runtime
       redirected to CamlinternalMenhirLib) *)
    | "parser", ".ml" -> None
    | "parser.pp", ".ml" -> Some "parser"
    | "zero_alloc_checker", ".mli" -> Some "mach_checks"
    | "cSE", (".cmi" | ".cmt" | ".cmx") -> Some "CSE"
    | "cSEgen", (".cmi" | ".cmt" | ".cmti" | ".cmx") -> Some "CSEgen"
    | ( "allowance" | "cmt2annot" | "compression" | "debug" | "diffing"
      | "diffing_with_keys" | "errortrace" | "file_sections" | "global_module"
      | "import_info" | "includemod_errorprinter"
      | "language_extension_kernel" | "lazy_backtrack" | "lru" | "main"
      | "main_native" | "maindriver" | "mode_hint" | "mode_intf" | "optmain"
      | "parser.pp"
      | "parser_types" | "shape" | "shape_reduce" | "signature_group"
      | "solver_intf" | "tmc" | "transl_array_comprehension"
      | "transl_comprehension_utils" | "transl_list_comprehension"
      | "unit_info" | "value_rec_types" ), ".mli" ->
      (* Preserve pre-pivot_root behaviour by leaving these out *)
      None
    | _, _ -> Some prefix
  in
  let tgt_dir_prefix =
    if String.starts_with ~prefix:"external/" path
    then ""
    else "compiler-libs/"
  in
  Option.map
    (fun tgt_prefix -> tgt_dir_prefix ^ tgt_prefix, extension)
    tgt_prefix_opt

let () =
  let paths = List.tl (Array.to_list Sys.argv) in
  let targets =
    List.filter_map
      (fun path -> Option.map (fun (tgt, ext) -> path, tgt, ext) (target path))
      paths
  in
  (* The .ml are installed so that the compiler libraries can be built for other
     targets from the installed compiler (see
     external/ast-dependent-libs/stdlib-jsir). The root directory also holds the
     sources of executables and intermediate files (config.common.ml, ...), so
     only install the .ml of units whose .cmi is installed. *)
  let units =
    List.filter_map
      (fun (_, tgt, ext) -> if ext = ".cmi" then Some tgt else None)
      targets
  in
  print_endline "(";
  List.iter
    (fun (path, tgt, ext) ->
      if ext <> ".ml" || List.mem tgt units
      then Printf.printf "(%s as %s%s)\n" path tgt ext)
    targets;
  print_endline ")"
