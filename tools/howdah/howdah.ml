(* Propagate a fatal error to stderr (caught at the end of this file). *)
exception Error_message of string

(* CLI info for the [Arg] module. *)
let flags : (string * Arg.spec * string) list = []
let description : string = "Extract a `.cmt{i}` file to Lean";;

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
  | Const_string (s, _, _) -> Json.String s |> tag "string" |> tag "String"
  | other_constant ->
    Format.asprintf "Constant not yet supported:@ %s"
      (Printpat.pretty_const other_constant)
    |> fun message -> raise (Error_message message)

(* Serialize an identifier. *)
let serialize_ident (id : Ident.t) : Json.t =
  Json.String (Ident.unique_toplevel_name id) |> tag "unique_toplevel_name"

let rec serialize_path ~(loc : Warnings.loc) : Path.t -> Json.t = function
  | Pident id -> serialize_ident id |> tag "id" |> tag "Ident"
  | Pdot (nested, field) ->
    Json.Object
      [ ("nested", serialize_path ~loc nested);
        ("field", Json.String field);
      ]
    |> tag "Dot"
  | other_path ->
    Format.asprintf "Path not yet supported:@ %a:@ %a"
      Location.print_loc loc
      Printtyp.path other_path
    |> fun message -> raise (Error_message message)

(* Serialize an expression. *)
let rec serialize_expr (expr : Typedtree.expression) : Json.t =
  let desc =
    begin match expr.exp_desc with
    | Texp_record { fields; _ } ->
      Array.to_list fields
      |> List.map serialize_record_label
      |> fun fields -> Json.Array fields
      |> tag "labels"
      |> tag "Record"
    | Texp_constant constant ->
      serialize_constant constant
      |> tag "const"
      |> tag "Constant"
    | _ ->
      Format.asprintf "Expression not yet supported:@ %a:@ %a"
        Location.print_loc expr.exp_loc
        Pprintast.expression Untypeast.(default_mapper.expr default_mapper expr)
      |> fun message -> raise (Error_message message)
    end
  in
  Json.Object
    [ ("desc", desc);
      ("type", serialize_type_expr ~loc:expr.exp_loc expr.exp_type);
    ]

(* Serialize a label within a record. *)
and serialize_record_label
    ( ({ lbl_name; lbl_loc; _ } : Data_types.label_description),
      (_ : Jkind_types.Sort.t),
      (defn : Typedtree.record_label_definition)) =
  begin match (defn : Typedtree.record_label_definition) with
  | Overridden (_loc, expr) ->
    Json.Object
      [ ("name", Json.String lbl_name);
        ("value", serialize_expr expr);
      ]
  | Kept _ ->
    Format.asprintf "Incomplete record expression not yet supported:@ %a"
      Location.print_loc lbl_loc
    |> fun message -> raise (Error_message message)
  end

(* Serialize a type expression. *)
and serialize_type_expr ~(loc : Warnings.loc) (ty : Types.type_expr) : Json.t =
  begin match Types.get_desc ty with
  | Tpoly (ty, []) -> serialize_type_expr ~loc ty
  | Tconstr (path, [], _) when Path.same path Predef.path_string ->
    Json.String "String"
  | Tconstr (path, [], _) ->
    serialize_path ~loc path |> tag "path" |> tag "Constr"
  | _ ->
    Format.asprintf "Type declaration not yet supported:@ %a:@ %a"
      Location.print_loc loc
      Printtyp.type_expr ty
    |> fun message -> raise (Error_message message)
  end

(* Serialize a value defined and bound to a name. *)
let serialize_value_binding (vb : Typedtree.value_binding) : Json.t =
  begin match vb.vb_pat.pat_desc with
  | Tpat_var { id; _ } ->
    Json.Object [("id", serialize_ident id); ("value", serialize_expr vb.vb_expr)]
  | _ ->
    Format.asprintf "Value binding not yet supported:@ %a:@ let %a =@ %a"
      Location.print_loc vb.vb_loc
      Printpat.Compat.pretty_pat vb.vb_pat
      Pprintast.expression Untypeast.(default_mapper.expr default_mapper vb.vb_expr)
    |> fun message -> raise (Error_message message)
  end

(* Serialize a label within a record. *)
let serialize_record_label_decl (label : Typedtree.label_declaration) : Json.t =
  Json.Object
    [ ("name", Json.String label.ld_name.txt);
      ("type", serialize_type_expr ~loc:label.ld_loc label.ld_type.ctyp_type);
    ]

(* Serialize a top-level type declaration. *)
let serialize_type_declaration (typedecl : Typedtree.type_declaration) : Json.t =
  let desc =
    begin match typedecl.typ_kind with
    | Ttype_record labels ->
      List.map serialize_record_label_decl labels
      |> fun labels -> tag "labels" (Json.Array labels)
      |> tag "record"
      |> tag "Record"
    | _ ->
      Format.asprintf "Type declaration not yet supported:@ %a:@ %a"
        Location.print_loc typedecl.typ_loc
        (Printtyp.type_declaration typedecl.typ_id) typedecl.typ_type
      |> fun message -> raise (Error_message message)
    end
  in
  Json.Object
    [ ("id", serialize_ident typedecl.typ_id);
      ("desc", desc)
    ]

(* Serialize a top-level item from an [.mli] file. *)
let serialize_signature_item (item : Typedtree.signature_item) : Json.t =
  begin match item.sig_desc with
  | _ ->
    item
    |> Untypeast.(default_mapper.signature_item default_mapper)
    |> Format.asprintf "Signature item not yet supported:@ %a:@ %a"
         Location.print_loc item.sig_loc
         Pprintast.signature_item
    |> fun message -> raise (Error_message message)
  end

(* Serialize a top-level item from an [.ml] file. *)
let serialize_structure_item (item : Typedtree.structure_item) : Json.t =
  begin match item.str_desc with
  | Tstr_type (_, types) ->
    List.map serialize_type_declaration types
    |> fun types -> Json.Array types
    |> tag "type_declarations"
    |> tag "TypeDeclarations"
  | Tstr_value (_, values) ->
    List.map serialize_value_binding values
    |> fun types -> Json.Array types
    |> tag "value_bindings"
    |> tag "ValueBindings"
  | _ ->
    item
    |> Untypeast.(default_mapper.structure_item default_mapper)
    |> Format.asprintf "Structure item not yet supported:@ %a:@ %a"
         Location.print_loc item.str_loc
         Pprintast.structure_item
    |> fun message -> raise (Error_message message)
  end

(* Handle a [.cmti] from an [.mli] file. *)
let run_signature ({ sig_items; _ } : Typedtree.signature) : Json.t =
  List.map serialize_signature_item sig_items
  |> fun items -> tag "items" (Json.Array items)

(* Handle a [.cmt] from an [.ml] file. *)
let run_structure ({ str_items; _ } : Typedtree.structure) : Json.t =
  List.map serialize_structure_item str_items
  |> fun items -> tag "items" (Json.Array items)

(* Handle the user's supplied (alleged) filename, whatever that may be. *)
let run (filename : string) : unit =
  begin match Filename.extension filename with
  | ".cmt" | ".cmti" -> ()
  | _ -> raise (Error_message "input file extension must be `.cmt` or `.cmti`")
  end;
  let info : Cmt_format.cmt_infos = Cmt_format.read_cmt filename in
  let json_output : Json.t =
    begin match info.cmt_annots with
    | Interface signature ->
      run_signature signature
      |> tag "sig"
      |> tag "Signature"
    | Implementation structure ->
      run_structure structure
      |> tag "str"
      |> tag "Structure"
    | Functorize | Packed _ | Partial_implementation _ | Partial_interface _ ->
      raise (Error_message "input `.cmt{i}` must come from a full `.ml{i}` file")
    end
  in
  Format.printf "%a@." (Json.print ~mode:Pretty) json_output

(* Parse command-line arguments and invoke the main program behavior. *)
let main () : unit =
  try Arg.parse_expand flags run description with
  | Error_message message ->
    Printf.fprintf stderr "ERROR: %s\n%!" message;
    exit 2

(* Run [main] immediately at toplevel. *)
let () = main ();;
