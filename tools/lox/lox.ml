(* CLI info for the [Arg] module. *)
let flags : (string * Arg.spec * string) list = []
let description : string = "Extract a matching `.cmti` and `.cmt` to Lean";;

(* Check that the [.cmti] and [.cmt] file discuss the same source code. *)
let check_from_same_compilation_unit
    (cmti : Cmt_format.cmt_infos) (cmt : Cmt_format.cmt_infos) : unit =
  let compilation_unit_name = Compilation_unit.name cmt.cmt_modname in
  let find_same_unit (import_info : Import_info.t) =
    let import_info_name = Import_info.name import_info in
    if Compilation_unit.Name.equal import_info_name compilation_unit_name
    then Import_info.crc import_info
    else None
  in
  let compilation_unit_in_cmt = Array.find_map find_same_unit cmt.cmt_imports in
  match cmti.cmt_interface_digest, compilation_unit_in_cmt with
  | Some intf, Some impl when Digest.equal intf impl -> ()
  | _, _ ->
    raise
      ( Error.Message
          "`.cmti` and `.cmt` were compiled from different units" )

let get_intf_from_cmti (cmti : Cmt_format.cmt_infos)
    : Typedtree.signature =
  begin match cmti.cmt_annots with
  | Interface signature -> signature
  | Functorize
  | Implementation _
  | Packed _
  | Partial_implementation _
  | Partial_interface _
    -> raise (Error.Message "input `.cmti` must come from a full `.mli` file")
  end

let get_impl_from_cmt (intf : Typedtree.signature) (cmt : Cmt_format.cmt_infos)
    : Compilation_unit.t list * Lambda.lambda =
  let structure : Typedtree.structure =
    begin match cmt.cmt_annots with
    | Implementation structure -> structure
    | Functorize
    | Interface _
    | Packed _
    | Partial_implementation _
    | Partial_interface _
      -> raise (Error.Message "input `.cmt` must come from a full `.ml` file")
    end
  in
  Load_path.init ~auto_include:Load_path.no_auto_include
    ~visible:cmt.cmt_loadpath.visible ~hidden:cmt.cmt_loadpath.hidden;
  let rehydrate =
    { Tast_mapper.default with
      env = fun _ -> Envaux.env_of_only_summary ~allow_missing_modules:true }
  in
  let structure = rehydrate.structure rehydrate structure in
  (* Reorder fields in the [.ml] as necessary to match the [.mli]. *)
  let (coercion, _) : (Typedtree.module_coercion * Shape.t) =
    let mode = Persistent_env.mode_pers_mod Mode.Staticity.Dynamic in
    Includemod.compunit
      (Envaux.env_of_only_summary cmt.cmt_initial_env)
      ~mark:false
      (Option.value cmt.cmt_sourcefile ~default:"")
      ~modes:(Includecore.Specific ((mode, None), mode))
      structure.str_type "(inferred signature)" intf.sig_type Shape.dummy_mod
  in
  let { code = tlambda; required_globals; _ } : Lambda.program =
    Translmod.transl_implementation cmt.cmt_modname
      (structure, coercion, None) ~loc:Location.none
  in
  let cu_static_data (compilation_unit : Compilation_unit.t) : 'a =
    Compilation_unit.full_path_as_string compilation_unit
    |> Printf.sprintf "Cross-module static evaluation not yet supported: %s"
    |> fun message -> raise (Error.Message message)
  in
  let (_, rawlambda) : (Slambda.CU_data.t * Lambda.lambda) =
    Slambda.eval ~cu_static_data tlambda
  in
  let lambda : Lambda.lambda =
    Simplif.simplify_lambda rawlambda
      ~restrict_to_upstream_dwarf:!Clflags.restrict_to_upstream_dwarf
      ~gdwarf_may_alter_codegen:false
  in
  (Compilation_unit.Set.elements required_globals, lambda)

(* Write [json] to [filename_without_extension], extending it with [suffix]. *)
let write_json_to_file (filename : string) (json : Json.t) : unit =
  let write channel =
    Format.fprintf (Format.formatter_of_out_channel channel) "%a@."
      (Json.print ~mode:Pretty) json
  in
  Out_channel.with_open_text filename write

let lean_file_contents ~(imports : string list) : string =
  let import (name : string) : string =
    "import extracted_oxcaml_" ^ String.uncapitalize_ascii name ^ "\n"
  in
  "import Lox.BuildSystem\n"
  ^ String.concat "" (List.map import imports)
  ^ "\n_do_not_use_this_outside_automatically_generated_files%\n"

(* Handle the user's supplied filenames. *)
let run ~(cmti_filename : string) ~(cmt_filename : string) : unit =
  if Filename.extension cmti_filename <> ".cmti" then
    raise (Error.Message "first input file extension must be `.cmti`");
  if Filename.extension cmt_filename <> ".cmt" then
    raise (Error.Message "second input file extension must be `.cmt`");
  let cmti = Cmt_format.read_cmt cmti_filename in
  let cmt = Cmt_format.read_cmt cmt_filename in
  check_from_same_compilation_unit cmti cmt;
  let intf = get_intf_from_cmti cmti in
  let (imports, impl) = get_impl_from_cmt intf cmt in
  let imports = List.map Compilation_unit.name_as_string imports in
  let intf = Convert.intf intf in
  let impl = Convert.impl impl in
  let filepath = Filename.remove_extension cmt_filename in
  let intf_filepath = filepath ^ ".intf.json" in
  let impl_filepath = filepath ^ ".impl.json" in
  let lean_filepath =
    "extracted_oxcaml_" ^ (Filename.basename filepath) ^ ".lean"
    |> Filename.concat (Filename.dirname cmt_filename)
  in
  Serialize.intf intf |> write_json_to_file intf_filepath;
  Serialize.impl impl |> write_json_to_file impl_filepath;
  Out_channel.with_open_text lean_filepath (fun channel ->
    Out_channel.output_string channel (lean_file_contents ~imports))

(* Parse command-line arguments and invoke the main program behavior. *)
let main () : unit =
  try
    let filenames = ref [] in
    let add_filename filename = filenames := filename :: !filenames in
    Arg.parse_expand flags add_filename description;
    begin match !filenames with
    | [cmt_filename; cmti_filename] ->
      (* reversed! (because of [::] above) *)
      run ~cmti_filename ~cmt_filename
    | _ -> raise (Error.Message "expected exactly two files: `.cmti` then `.cmt`")
    end
  with
  | Error.Message message ->
    Printf.fprintf stderr "ERROR: %s\n%!" message;
    exit 2

(* Run [main] immediately at toplevel. *)
let () = main ();;
