(* Propagate a fatal error to stderr (caught at the end of this file). *)
exception Error_message of string

(* CLI info for the [Arg] module. *)
let flags : (string * Arg.spec * string) list = []
let description : string = "Extract a matching `.cmi` and `.cmt` to Lean";;

(* Serialize a Lean constructor to JSON by tagging its constructor name. *)
let tag (lean_ctor_name : string) (contents : Json.t) : Json.t =
  Json.Object [(lean_ctor_name, contents)]

(* Original [ocamlcmt] implementation. *)
(*
Compmisc.init_path ();
let cmt : Cmt_format.cmt_infos = Cmt_format.read_cmt filename in
Envaux.reset_cache ~preserve_persistent_env:false;
List.iter (Load_path.add_dir ~hidden:false) cmt.cmt_loadpath.visible;
List.iter (Load_path.add_dir ~hidden:true) cmt.cmt_loadpath.hidden;
Cmt2annot.gen_annot target_filename
  ~sourcefile:cmt.cmt_sourcefile
  ~use_summaries:cmt.cmt_use_summaries
  cmt.cmt_annots
*)

(* Serialize a constant expression. *)
let serialize_constant : Typedtree.constant -> Json.t = function
  | Const_string (s, _, _) ->
    String.to_seq s
    |> Seq.map (fun c -> Char.code c |> Json.int)
    |> List.of_seq
    |> fun bytes -> Json.Array bytes
    |> tag "bytes"
    |> tag "string"
  | other_constant ->
    Format.asprintf "Constant not yet supported:@ %s"
      (Printpat.pretty_const other_constant)
    |> fun message -> raise (Error_message message)

(* Serialize an identifier. *)
let serialize_ident (id : Ident.t) : Json.t =
  Json.String (Ident.unique_toplevel_name id) |> tag "uniqueToplevelName"

(* Serialize a source-code path like [F(X).a.b.c]. *)
let rec serialize_path ~(loc : Warnings.loc) : Path.t -> Json.t = function
  | Pident id -> serialize_ident id |> tag "id" |> tag "ident"
  | Pdot (nested, field) ->
    Json.Object
      [ ("nested", serialize_path ~loc nested);
        ("field", Json.String field);
      ]
    |> tag "dot"
  | other_path ->
    Format.asprintf "Path not yet supported:@ %a:@ %a"
      Location.print_loc loc
      Printtyp.path other_path
    |> fun message -> raise (Error_message message)

(* Serialize a constant in [Lambda]. *)
let rec serialize_structured_constant
  : Lambda.structured_constant -> Json.t = function
  | Const_base constant ->
    serialize_constant constant
    |> tag "const"
    |> tag "base"
  | Const_block (block_tag, fields) ->
    Json.Object
      [ ("tag", Json.int block_tag);
        ("fields", Json.Array (List.map serialize_structured_constant fields));
      ]
    |> tag "block"
  | other_constant ->
    Format.asprintf "Structured constant not yet supported:@ %a"
      Printlambda.structured_constant other_constant
    |> fun message -> raise (Error_message message)

(* Serialize an untyped [Lambda] expression. *)
let rec serialize_lambda : Lambda.lambda -> Json.t = function
  | Lvar id ->
    serialize_ident id
    |> tag "id"
    |> tag "var"
  | Lconst constant ->
    serialize_structured_constant constant
    |> tag "const"
    |> tag "const"
  | Llet (Strict, _, id, _, value, body) ->
    Json.Object
      [ ("id", serialize_ident id);
        ("subst", serialize_lambda value);
        ("body", serialize_lambda body);
      ]
    |> tag "bind"
  | Lprim (prim, args, _) ->
    Json.Object
      [ ("prim", serialize_primitive prim);
        ("args", Json.Array (List.map serialize_lambda args));
      ]
    |> tag "prim"
  | other_lambda ->
    Format.asprintf "Lambda term not yet supported:@ %a"
      Printlambda.lambda other_lambda
    |> fun message -> raise (Error_message message)

(* Serialize a [Lambda] primitive. *)
and serialize_primitive : Lambda.primitive -> Json.t = function
  | Pmakeblock (block_tag, Immutable, _, Alloc_heap) ->
    Json.int block_tag
    |> tag "tag"
    |> tag "makeBlock"
  | other_primitive ->
    Format.asprintf "Lambda primitive not yet supported:@ %a"
      Printlambda.primitive other_primitive
    |> fun message -> raise (Error_message message)

(* Serialize a type expression. *)
let rec serialize_type_expr ~(loc : Warnings.loc) (ty : Types.type_expr) : Json.t =
  begin match Types.get_desc ty with
  | Tpoly (ty, []) -> serialize_type_expr ~loc ty
  | Tconstr (path, [], _) when Path.same path Predef.path_string ->
    Json.String "string"
  | Tconstr (path, [], _) ->
    serialize_path ~loc path |> tag "path" |> tag "constr"
  | _ ->
    Format.asprintf "Type declaration not yet supported:@ %a:@ %a"
      Location.print_loc loc
      Printtyp.type_expr ty
    |> fun message -> raise (Error_message message)
  end

(* Serialize a label within a record. *)
let serialize_record_label_decl (label : Types.label_declaration) : Json.t =
  Json.Object
    [ ("id", serialize_ident label.ld_id);
      ("type", serialize_type_expr ~loc:label.ld_loc label.ld_type);
    ]

(* Serialize a top-level type declaration. *)
let serialize_type_declaration (id : Ident.t) (decl : Types.type_declaration)
  : Json.t =
  let desc =
    begin match decl.type_kind with
    | Type_record (labels, _, _) ->
      List.map serialize_record_label_decl labels
      |> fun labels -> tag "labels" (Json.Array labels)
      |> tag "record"
      |> tag "record"
    | _ ->
      Format.asprintf "Type declaration not yet supported:@ %a:@ %a"
        Location.print_loc decl.type_loc
        (Printtyp.type_declaration id) decl
      |> fun message -> raise (Error_message message)
    end
  in
  Json.Object
    [ ("id", serialize_ident id);
      ("desc", desc)
    ]

(* Serialize a top-level item from an [.mli] file. *)
let serialize_signature_item : Types.signature_item -> Json.t = function
  | Sig_type (id, decl, _, Exported) ->
    serialize_type_declaration id decl
    |> tag "decl"
    |> tag "typeDecl"
  | Sig_value (id, desc, Exported) ->
    Json.Object
      [ ("id", serialize_ident id);
        ("type", serialize_type_expr ~loc:desc.val_loc desc.val_type);
      ]
    |> tag "exprDecl"
  | other_item ->
    Format.asprintf "Signature item not yet supported:@ %a"
      Printtyp.signature [other_item]
    |> fun message -> raise (Error_message message)

(* Serialize an interface from an [.mli]/[.cmi] pair. *)
let serialize_interface (signature : Types.signature) : Json.t =
  List.map serialize_signature_item signature
  |> fun items -> tag "items" (Json.Array items)

(* Serialize an implementation from an [.ml]/[.cmt] pair. *)
let serialize_implementation
    (info : Cmt_format.cmt_infos) (structure : Typedtree.structure)
    (coercion : Typedtree.module_coercion) : Json.t =
  let { code = tlambda; _ } : Lambda.program =
    Translmod.transl_implementation info.cmt_modname
      (structure, coercion, None) ~loc:Location.none
  in
  let cu_static_data compilation_unit =
    Compilation_unit.full_path_as_string compilation_unit
    |> Printf.sprintf "Cross-module static evaluation not yet supported: %s"
    |> fun message -> raise (Error_message message)
  in
  let _static_data, lambda =
    Slambda.eval ~cu_static_data Fun.id tlambda
  in
  serialize_lambda lambda

(* Restore load-path information from the [.cmt] artifact. *)
let rehydrate_envs (info : Cmt_format.cmt_infos) :
    Typedtree.structure -> Typedtree.structure =
  Load_path.init ~auto_include:Load_path.no_auto_include
    ~visible:info.cmt_loadpath.visible ~hidden:info.cmt_loadpath.hidden;
  let mapper =
    { Tast_mapper.default with
      env = fun _ env -> Envaux.env_of_only_summary env }
  in
  mapper.structure mapper

(* Check that the [.cmi] and [.cmt] file discuss the same source code. *)
let check_same_compilation
    (cmi : Cmi_format.cmi_infos) (cmt : Cmt_format.cmt_infos) : unit =
  let cmi_crc =
    Array.find_map
      ( fun import ->
          if Compilation_unit.Name.equal (Import_info.name import) cmi.cmi_name
          then Import_info.crc import
          else None )
      cmi.cmi_crcs
  in
  let digest =
    begin match cmt.cmt_interface_digest with
    | Some digest -> digest
    | None ->
      raise
        (Error_message "`.cmt` without an inferred interface not yet supported")
    end
  in
  if Some digest <> cmi_crc
  then begin
    raise (Error_message "`.cmi` and `.cmt` come from different compilations")
  end

(* Handle the user's supplied (alleged) filenames, whatever those may be. *)
let run ~(cmi_filename : string) ~(cmt_filename : string) : unit =
  if Filename.extension cmi_filename <> ".cmi" then
    raise (Error_message "first input file extension must be `.cmi`");
  if Filename.extension cmt_filename <> ".cmt" then
    raise (Error_message "second input file extension must be `.cmt`");
  let cmi = Cmi_format.read_cmi cmi_filename in
  let cmt = Cmt_format.read_cmt cmt_filename in
  check_same_compilation cmi cmt;
  let interface, _ = cmi.cmi_sign in
  let structure =
    begin match cmt.cmt_annots with
    | Implementation structure -> rehydrate_envs cmt structure
    | Interface _ | Functorize | Packed _
    | Partial_implementation _ | Partial_interface _ ->
      raise (Error_message "input `.cmt` must come from a full `.ml` file")
    end
  in
  (* Reorder fields in the [.ml] as necessary to match the [.mli]. *)
  let coercion, _ =
    let mode = Persistent_env.mode_pers_mod Mode.Staticity.Dynamic in
    Includemod.compunit
      (Envaux.env_of_only_summary cmt.cmt_initial_env)
      ~mark:false
      (Option.value cmt.cmt_sourcefile ~default:"")
      ~modes:(Includecore.Specific ((mode, None), mode))
      structure.str_type "(inferred signature)" interface Shape.dummy_mod
  in
  let json_output : Json.t =
    Json.Object
      [ ("interface", serialize_interface interface);
        ("implementation", serialize_implementation cmt structure coercion);
      ]
  in
  Format.printf "%a@." (Json.print ~mode:Pretty) json_output

(* Parse command-line arguments and invoke the main program behavior. *)
let main () : unit =
  try
    let filenames = ref [] in
    let add_filename filename = filenames := filename :: !filenames in
    Arg.parse_expand flags add_filename description;
    begin match !filenames with
    | [cmt_filename; cmi_filename] ->
      (* reversed! (because of [::] above) *)
      run ~cmi_filename ~cmt_filename
    | _ -> raise (Error_message "expected exactly two files: `.cmi` then `.cmt`")
    end
  with
  | Error_message message ->
    Printf.fprintf stderr "ERROR: %s\n%!" message;
    exit 2

(* Run [main] immediately at toplevel. *)
let () = main ();;
