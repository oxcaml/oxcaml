(* Propagate a fatal error to stderr (caught at the end of this file). *)
exception Error_message of string

(* CLI info for the [Arg] module. *)
let flags : (string * Arg.spec * string) list = []
let description : string = "Extract a matching `.cmti` and `.cmt` to Lean";;

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
  | Const_int i -> Json.int i |> tag "bits" |> tag "int"
  | other_constant ->
    Format.asprintf "Constant not yet supported:@ %s"
      (Printpat.pretty_const other_constant)
    |> fun message -> raise (Error_message message)

(* Serialize an identifier. *)
let serialize_ident (id : Ident.t) : Json.t =
  let name = Ident.name id in
  let stamp =
    if Ident.is_predef id then Json.String "predef" else
    if Ident.is_global id then Json.String "global" else
    Json.Object [("stamp", Ident.stamp id |> Json.int)]
    |> tag "stamped"
  in
  Json.Object
    [ ("name", Json.String name);
      ("stamp", stamp);
    ]

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
  | Lapply { ap_func; ap_args; _ } ->
    Json.Object
      [ ("func", serialize_lambda ap_func);
        ("args", Json.Array (List.map serialize_lambda ap_args));
      ]
    |> tag "apply"
  | Llet (Strict, _, id, _, value, body) ->
    Json.Object
      [ ("id", serialize_ident id);
        ("bound", serialize_lambda value);
        ("body", serialize_lambda body);
      ]
    |> tag "letBinding"
  | Lprim (prim, args, _) ->
    Json.Object
      [ ("prim", serialize_primitive prim);
        ("args", Json.Array (List.map serialize_lambda args));
      ]
    |> tag "prim"
  | Lfunction f ->
    serialize_function f |> tag "function" |> tag "function"
  | Lifthenelse (condition, body_if_true, body_if_false, _) ->
    Json.Object
      [ ("condition", serialize_lambda condition);
        ("bodyIfTrue", serialize_lambda body_if_true);
        ("bodyIfFalse", serialize_lambda body_if_false);
      ]
    |> tag "ifThenElse"
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
  | Pfield (index, _, Reads_agree) ->
    Json.int index
    |> tag "index"
    |> tag "field"
  | Pphys_equal Eq -> Json.String "physEqual"
  | Pscalar (Binary (Integral (Value (Taggable Int), Mod (Safe, Signed)))) ->
    Json.String "intMod"
  | other_primitive ->
    Format.asprintf "Lambda primitive not yet supported:@ %a"
      Printlambda.primitive other_primitive
    |> fun message -> raise (Error_message message)

(* Serialize an untyped [Lambda] function. *)
and serialize_function (f : Lambda.lfunction) : Json.t =
  match f.params with
  | headParam :: tailParams ->
    Json.Object
      [ ( "headParam", serialize_ident headParam.name );
        ( "tailParams",
          Json.Array
            ( List.map
                (fun (p : Lambda.lparam) -> serialize_ident p.name)
                tailParams ) );
        ( "body", serialize_lambda f.body );
      ]
  | [] ->
    Format.asprintf "Function with zero arguments:@ %a:@ %a"
      Location.print_loc (Debuginfo.Scoped_location.to_location f.loc)
      Printlambda.lfunction f
    |> fun message -> raise (Error_message message)

(* Serialize a type expression. *)
let rec serialize_type_expr ~(loc : Warnings.loc) (ty : Types.type_expr) : Json.t =
  begin match Types.get_desc ty with
  (* CR wsturgeon: remove the below special case *)
  | Tpoly (ty, []) -> serialize_type_expr ~loc ty
  (* CR wsturgeon: remove the below special case *)
  | Tconstr (path, [], _) when Path.same path Predef.path_string ->
    Json.String "string"
  | Tconstr (path, args, _) ->
    Json.Object
      [ ("path", serialize_path ~loc path);
        ("args", Json.Array (List.map (serialize_type_expr ~loc) args));
      ]
    |> tag "constr"
  | Tarrow (_, lhs, rhs, _) ->
    Json.Object
      [ ("lhs", serialize_type_expr ~loc lhs);
        ("rhs", serialize_type_expr ~loc rhs);
      ]
    |> tag "arrow"
  | _ ->
    Format.asprintf "Type expression not yet supported:@ %a:@ %a"
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
    | Type_abstract Definition -> Json.String "abstract"
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

(* Serialize an interface from an [.mli]/[.cmti] pair. *)
let rec serialize_signature (signature : Types.signature) : Json.t =
  List.map serialize_signature_item signature
  |> fun items -> tag "items" (Json.Array items)

(* Serialize a top-level item from an [.mli] file. *)
and serialize_signature_item : Types.signature_item -> Json.t = function
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
  | Sig_module (id, _, decl, _, _) ->
    Json.Object
      [ ("id", serialize_ident id);
        ("type", serialize_module_type ~loc:decl.md_loc decl.md_type);
      ]
    |> tag "moduleDecl"
  | Sig_law (id, _, _) -> tag "id" (serialize_ident id) |> tag "law"
  | other_item ->
    Format.asprintf "Signature item not yet supported:@ %a"
      Printtyp.signature [other_item]
    |> fun message -> raise (Error_message message)

(* Serialize a top-level module type from an [.mli] file. *)
and serialize_module_type ~(loc : Warnings.loc) :
    Types.module_type -> Json.t = function
  | Mty_signature s -> serialize_signature s |> tag "signature"
  | other_module_type ->
    Format.asprintf "Module type not yet supported:@ %a:@ %a"
      Location.print_loc loc
      Printtyp.modtype other_module_type
    |> fun message -> raise (Error_message message)

(* Serialize an implementation from an [.ml]/[.cmt] pair. *)
let serialize_structure
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
  let _, rawlambda = Slambda.eval ~cu_static_data Fun.id tlambda in
  let lambda =
    Simplif.simplify_lambda rawlambda
      ~restrict_to_upstream_dwarf:!Clflags.restrict_to_upstream_dwarf
      ~gdwarf_may_alter_codegen:false
  in
  serialize_lambda lambda

let init_load_path (info : Cmt_format.cmt_infos) : unit =
  Load_path.init ~auto_include:Load_path.no_auto_include
    ~visible:info.cmt_loadpath.visible ~hidden:info.cmt_loadpath.hidden

let rehydrate : Tast_mapper.mapper =
  { Tast_mapper.default with
    env = fun _ env -> Envaux.env_of_only_summary env }

let rec check_no_local_types ~(loc : Warnings.loc) (ty : Types.type_expr) =
  match Types.get_desc ty with
  | Tconstr (path, args, _) ->
    let is_external id = Ident.is_predef id || Ident.is_global id in
    if List.for_all is_external (Path.heads path) then
      List.iter (check_no_local_types ~loc) args
    else
      Format.asprintf "Local type in an abstract type's definition:@ %a:@ %a"
        Location.print_loc loc Printtyp.type_expr ty
      |> fun message -> raise (Error_message message)
  | Tarrow (_, lhs, rhs, _) ->
    check_no_local_types ~loc lhs;
    check_no_local_types ~loc rhs
  | _ -> ()

let serialize_abstract_type_definitions
    ~(interface : Types.signature) ~(implementation : Types.signature)
  : Json.t =
  let definition_in_implementation name =
    List.find_map
      (function
        | Types.Sig_type (id, decl, _, _) when Ident.name id = name -> Some decl
        | _ -> None)
      implementation
  in
  let serialize_definition : Types.signature_item -> Json.t option = function
    | Sig_type
        (id, { type_kind = Type_abstract _; type_manifest = None; _ }, _, _) ->
      begin match definition_in_implementation (Ident.name id) with
      | Some { type_manifest = Some ty; type_params = []; type_loc = loc; _ } ->
        check_no_local_types ~loc ty;
        Some
          (Json.Object
             [ ("id", serialize_ident id);
               ("definition", serialize_type_expr ~loc ty);
             ])
      | _ ->
        Printf.sprintf
          "Abstract type `%s` must be a non-parameterized alias in the `.ml`"
          (Ident.name id)
        |> fun message -> raise (Error_message message)
      end
    | _ -> None
  in
  Json.Array (List.filter_map serialize_definition interface)

let field_positions (interface : Types.signature) : int Ident.Map.t =
  List.filter Includemod.is_runtime_component interface
  |> List.mapi (fun position item -> Types.signature_item_id item, position)
  |> List.to_seq
  |> Ident.Map.of_seq

let serialize_law ~(positions : int Ident.Map.t)
    (law : Typedtree.law_declaration) : Json.t =
  let self = Ident.create_local "self" in
  let project_from_self _ position : Lambda.lambda =
    Lprim
      ( Pfield (position, Pointer, Reads_agree),
        [Lvar self],
        Debuginfo.Scoped_location.Loc_unknown )
  in
  let components = Ident.Map.mapi project_from_self positions in
  let params =
    List.map (fun (param : Typedtree.law_param) -> param.lp_id) law.law_params
  in
  let serialize_clause (clause : Typedtree.spec_expression) : Json.t =
    Translspec.typedtree clause
    |> rehydrate.expr rehydrate
    |> Translcore.transl_exp ~scopes:Debuginfo.Scoped_location.empty_scopes
         Lambda.layout_int
    |> Lambda.subst (fun _ _ env -> env) components
    |> fun body ->
    Json.Object
      [ ("headParam", serialize_ident self);
        ("tailParams", Json.Array (List.map serialize_ident params));
        ("body", serialize_lambda body);
      ]
  in
  let serialize_param (param : Typedtree.law_param) : Json.t =
    Json.Object
      [ ("id", serialize_ident param.lp_id);
        ( "type",
          serialize_type_expr ~loc:param.lp_type.ctyp_loc
            param.lp_type.ctyp_type );
      ]
  in
  Json.Object
    [ ("name", Json.String law.law_name.txt);
      ("params", Json.Array (List.map serialize_param law.law_params));
      ( "assumptions",
        Json.Array (List.map serialize_clause law.law_assumptions) );
      ("conclusion", serialize_clause law.law_conclusion);
    ]

(* Check that the [.cmti] and [.cmt] file discuss the same source code. *)
let check_same_compilation
    ~(cmti : Cmt_format.cmt_infos) ~(cmt : Cmt_format.cmt_infos) : unit =
  let unit_name = Compilation_unit.name cmti.cmt_modname in
  let interface_crc_seen_by_implementation =
    Array.find_map
      ( fun import ->
          if Import_info.name import |> Compilation_unit.Name.equal unit_name
          then Import_info.crc import
          else None )
      cmt.cmt_imports
  in
  match interface_crc_seen_by_implementation, cmti.cmt_interface_digest with
  | Some seen, Some actual when Digest.equal seen actual -> ()
  | _ ->
    raise
      ( Error_message
          "`.cmti` and `.cmt` were compiled from different units" )

(* Handle the user's supplied (alleged) filenames, whatever those may be. *)
let run ~(cmti_filename : string) ~(cmt_filename : string) : unit =
  if Filename.extension cmti_filename <> ".cmti" then
    raise (Error_message "first input file extension must be `.cmti`");
  if Filename.extension cmt_filename <> ".cmt" then
    raise (Error_message "second input file extension must be `.cmt`");
  let cmti = Cmt_format.read_cmt cmti_filename in
  let cmt = Cmt_format.read_cmt cmt_filename in
  check_same_compilation ~cmti ~cmt;
  init_load_path cmt;
  let signature =
    begin match cmti.cmt_annots with
    | Interface signature -> rehydrate.signature rehydrate signature
    | Implementation _ | Functorize | Packed _
    | Partial_implementation _ | Partial_interface _ ->
      raise (Error_message "input `.cmti` must come from a full `.mli` file")
    end
  in
  let structure =
    begin match cmt.cmt_annots with
    | Implementation structure -> rehydrate.structure rehydrate structure
    | Interface _ | Functorize | Packed _
    | Partial_implementation _ | Partial_interface _ ->
      raise (Error_message "input `.cmt` must come from a full `.ml` file")
    end
  in
  let interface = signature.sig_type in
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
  let laws =
    List.filter_map
      (fun (item : Typedtree.signature_item) ->
        match item.sig_desc with
        | Tsig_law law -> Some law
        | _ -> None)
      signature.sig_items
  in
  let positions = field_positions interface in
  let json_output : Json.t =
    Json.Object
      [ ( "unitName",
          Json.String (Compilation_unit.full_path_as_string cmt.cmt_modname) );
        ("interface", serialize_signature interface);
        ( "abstractTypeDefinitions",
          serialize_abstract_type_definitions ~interface
            ~implementation:structure.str_type );
        ("laws", Json.Array (List.map (serialize_law ~positions) laws));
        ("implementation", serialize_structure cmt structure coercion);
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
    | [cmt_filename; cmti_filename] ->
      (* reversed! (because of [::] above) *)
      run ~cmti_filename ~cmt_filename
    | _ ->
      raise (Error_message "expected exactly two files: `.cmti` then `.cmt`")
    end
  with
  | Error_message message ->
    Printf.fprintf stderr "ERROR: %s\n%!" message;
    exit 2

(* Run [main] immediately at toplevel. *)
let () = main ();;
