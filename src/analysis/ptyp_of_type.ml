open Std
open Typedtree
open Types

let var_of_id id = Location.mknoloc @@ Ident.name id

type signature_elt =
  | Item of Types.signature_item
  | Type of Asttypes.rec_flag * Parsetree.type_declaration list

let rec module_type =
  let open Ast_helper in
  function
  | Mty_for_hole -> failwith "Holes are not allowed in module types"
  | Mty_signature signature_items -> Mty.signature @@ signature signature_items
  | Mty_ident path ->
    Ast_helper.Mty.ident (Location.mknoloc (Untypeast.lident_of_path path))
  | Mty_alias path ->
    Ast_helper.Mty.alias (Location.mknoloc (Untypeast.lident_of_path path))
  | Mty_functor (param, type_out) ->
    let param =
      match param with
      | Unit -> Parsetree.Unit
      | Named (id, type_in) ->
        Parsetree.Named
          ( Location.mknoloc (Option.map ~f:Ident.name id),
            module_type type_in,
            [] )
    in
    let out = module_type type_out in
    Mty.functor_ param out
  | Mty_strengthen (mty, path, _aliasability) ->
    Mty.strengthen ~loc:Location.none (module_type mty)
      (Location.mknoloc (Untypeast.lident_of_path path))

and core_type type_expr =
  let open Ast_helper in
  match Types.get_desc type_expr with
  | Tvar { name = None; jkind = _ } | Tunivar { name = None; jkind = _ } ->
    (* CR modes: do something better here with the jkind *)
    Typ.any None
  | Tvar { name = Some s; jkind = _ } | Tunivar { name = Some s; jkind = _ } ->
    (* CR modes: do something better here with the jkind *)
    Typ.var s None
  | Tarrow
      ( (label, arg_alloc_mode, ret_alloc_mode),
        type_expr,
        type_expr_out,
        _commutable ) ->
    let (label : Asttypes.arg_label), type_expr =
      match label with
      | Position l ->
        (Labelled l, Typ.extension (mkloc "call_pos" !default_loc, PStr []))
      | Nolabel -> (Nolabel, core_type type_expr)
      | Labelled l -> (Labelled l, core_type type_expr)
      | Optional l -> (Optional l, core_type type_expr)
    in
    let snap = Btype.snapshot () in
    let arg_modes =
      Typemode.untransl_mode_annots
      @@ Mode.Alloc.(Const.diff (zap_to_legacy arg_alloc_mode) Const.legacy)
    in
    let ret_modes =
      Typemode.untransl_mode_annots
      @@ Mode.Alloc.(Const.diff (zap_to_legacy ret_alloc_mode) Const.legacy)
    in
    Btype.backtrack snap;
    Typ.arrow label type_expr (core_type type_expr_out) arg_modes ret_modes
  | Ttuple type_exprs ->
    let labeled_type_exprs =
      List.map ~f:(fun (lbl, ty) -> (lbl, core_type ty)) type_exprs
    in
    Typ.tuple ~loc:!default_loc labeled_type_exprs
  | Tunboxed_tuple type_exprs ->
    let labeled_type_exprs =
      List.map ~f:(fun (lbl, ty) -> (lbl, core_type ty)) type_exprs
    in
    Typ.unboxed_tuple labeled_type_exprs
  | Tconstr (path, type_exprs, _abbrev) ->
    let loc = Untypeast.lident_of_path path |> Location.mknoloc in
    Typ.constr loc @@ List.map ~f:core_type type_exprs
  | Tobject (type_expr, _class_) ->
    let rec aux acc type_expr =
      match get_desc type_expr with
      | Tnil -> (acc, Asttypes.Closed)
      | Tvar { name = None; _ } | Tunivar { name = None; _ } ->
        (acc, Asttypes.Open)
      | Tfield ("*dummy method*", _, _, fields) -> aux acc fields
      | Tfield (name, _, type_expr, fields) ->
        let open Ast_helper in
        let core_type = core_type type_expr in
        let core_type = Of.tag (Location.mknoloc name) core_type in

        aux (core_type :: acc) fields
      | _ ->
        failwith
        @@ Format.asprintf "Unexpected type constructor in fields list: %a"
             Printtyp.type_expr type_expr
    in
    let fields, closed = aux [] type_expr in
    Typ.object_ fields closed
  | Tfield _ -> failwith "Found object field outside of object."
  | Tnil -> Typ.object_ [] Closed
  | Tlink type_expr | Tsubst (type_expr, _) -> core_type type_expr
  | Tvariant row ->
    let row_fields = row_fields row in
    let row_closed = row_closed row in
    let field (label, row_field) =
      let label = Location.mknoloc label in
      match row_field_repr row_field with
      | Rpresent None | Reither (true, _, _) -> Rf.tag label true []
      | Rpresent (Some type_expr) ->
        let core_type = core_type type_expr in
        Rf.tag label false [ core_type ]
      | Reither (false, type_exprs, _) ->
        Rf.tag label false @@ List.map ~f:core_type type_exprs
      | Rabsent -> assert false
    in
    let closed = if row_closed then Asttypes.Closed else Asttypes.Open in
    let fields = List.map ~f:field row_fields in
    (* TODO NOT ALWAYS NONE *)
    Typ.variant fields closed None
  | Tpoly (type_expr, type_exprs) ->
    let names =
      List.map
        ~f:(fun v ->
          match get_desc v with
          | Tunivar { name = Some name; jkind = _ }
          | Tvar { name = Some name; jkind = _ } ->
            (* CR modes: do something  *)
            (mknoloc name, None)
          | _ -> failwith "poly: not a var")
        type_exprs
    in
    Typ.poly names @@ core_type type_expr
  | Tof_kind _jkind -> (* CR modes: this is terrible *) Typ.any None
  | Tquote type_expr -> Typ.quote (core_type type_expr)
  | Tsplice type_expr -> Typ.splice (core_type type_expr)
  | Tpackage (path, lids_type_exprs) ->
    let loc = mknoloc (Untypeast.lident_of_path path) in
    let args =
      List.map lids_type_exprs ~f:(fun (id, t) -> (mknoloc id, core_type t))
    in
    Typ.package loc args

and modtype_declaration id { mtd_type; mtd_attributes; _ } =
  Ast_helper.Mtd.mk ~attrs:mtd_attributes
    ?typ:(Option.map ~f:module_type mtd_type)
    (var_of_id id)

and module_declaration id { md_type; md_attributes; _ } =
  let name = Location.mknoloc (Some (Ident.name id)) in
  Ast_helper.Md.mk ~attrs:md_attributes name @@ module_type md_type

and extension_constructor id { ext_args; ext_ret_type; ext_attributes; _ } =
  Ast_helper.Te.decl ~attrs:ext_attributes
    ~args:(constructor_arguments ext_args)
    ?res:(Option.map ~f:core_type ext_ret_type)
    (var_of_id id)

and const_modalities modalities =
  Typemode.untransl_modalities Immutable modalities

and value_description id
    { val_type; val_kind = _; val_loc; val_attributes; val_modalities; _ } =
  let type_ = core_type val_type in
  let snap = Btype.snapshot () in
  let modalities = Mode.Modality.zap_to_id val_modalities in
  Btype.backtrack snap;
  { Parsetree.pval_name = var_of_id id;
    pval_type = type_;
    pval_prim = [];
    pval_attributes = val_attributes;
    pval_modalities = const_modalities modalities;
    pval_loc = val_loc
  }

and constructor_argument { ca_type; ca_loc; ca_modalities; ca_sort = _ } =
  { Parsetree.pca_type = core_type ca_type;
    pca_loc = ca_loc;
    pca_modalities = const_modalities ca_modalities
  }

and label_declaration
    { ld_id; ld_mutable; ld_type; ld_attributes; ld_modalities; _ } =
  Ast_helper.Type.field ~attrs:ld_attributes
    ~mut:
      (match ld_mutable with
      | Mutable _ -> Mutable
      | Immutable -> Immutable)
    ~modalities:(Typemode.untransl_modalities ld_mutable ld_modalities)
    (var_of_id ld_id) (core_type ld_type)

and constructor_arguments = function
  | Cstr_tuple type_exprs ->
    Parsetree.Pcstr_tuple (List.map ~f:constructor_argument type_exprs)
  | Cstr_record label_decls ->
    Parsetree.Pcstr_record (List.map ~f:label_declaration label_decls)

and constructor_declaration { cd_id; cd_args; cd_res; cd_attributes; _ } =
  Ast_helper.Type.constructor ~attrs:cd_attributes
    ~args:(constructor_arguments cd_args)
    ?res:(Option.map ~f:core_type cd_res)
  @@ var_of_id cd_id

and type_declaration id
    { type_params;
      type_variance;
      type_manifest;
      type_kind;
      type_attributes;
      type_private;
      _
    } =
  let params =
    List.map2 type_params type_variance ~f:(fun type_ variance ->
        let core_type = core_type type_ in
        let pos, neg, inj = Types.Variance.get_lower variance in
        let v =
          if pos then Asttypes.Covariant
          else if neg then Contravariant
          else NoVariance
        in
        let i = if inj then Asttypes.Injective else NoInjectivity in
        (core_type, (v, i)))
  in
  let kind =
    match type_kind with
    | Type_abstract _ -> Parsetree.Ptype_abstract
    | Type_open -> Ptype_open
    | Type_variant (constrs, _, _) ->
      Ptype_variant (List.map ~f:constructor_declaration constrs)
    | Type_record (labels, _repr, _) ->
      Ptype_record (List.map ~f:label_declaration labels)
    | Type_record_unboxed_product (labels, _repr, _) ->
      Ptype_record_unboxed_product (List.map ~f:label_declaration labels)
  in
  let manifest = Option.map ~f:core_type type_manifest in
  Ast_helper.Type.mk ~attrs:type_attributes ~params ~kind ~priv:type_private
    ?manifest (var_of_id id)

and signature_item (str_item : Types.signature_item) =
  let open Ast_helper in
  match str_item with
  | Sig_value (id, vd, _visibility) ->
    let vd = value_description id vd in
    Sig.value vd
  | Sig_type (id, type_decl, rec_flag, _visibility) ->
    let rec_flag =
      match rec_flag with
      | Trec_first -> Asttypes.Recursive
      | Trec_next -> Asttypes.Recursive
      | Trec_not -> Nonrecursive
    in
    (* mutually recursive types are really handled by [signature] *)
    Sig.type_ rec_flag [ type_declaration id type_decl ]
  | Sig_modtype (id, modtype_decl, _visibility) ->
    Sig.modtype @@ modtype_declaration id modtype_decl
  | Sig_module (id, _, mod_decl, _, _) ->
    Sig.module_ @@ module_declaration id mod_decl
  | Sig_typext (id, ext_constructor, _, _) ->
    let ext =
      Te.mk
        (Location.mknoloc @@ Longident.Lident (Ident.name id))
        [ extension_constructor id ext_constructor ]
    in
    Sig.type_extension ext
  | Sig_class_type (id, _, _, _) ->
    let str =
      Format.asprintf
        "Construct does not handle class types yet. Please replace this \
         comment by [%s]'s definition."
        (Ident.name id)
    in
    Sig.text [ Docstrings.docstring str Location.none ] |> List.hd
  | Sig_class (id, _, _, _) ->
    let str =
      Format.asprintf
        "Construct does not handle classes yet. Please replace this comment by \
         [%s]'s definition."
        (Ident.name id)
    in
    Sig.text [ Docstrings.docstring str Location.none ] |> List.hd

and signature (items : Types.signature) =
  Ast_helper.Sg.mk
    (List.map (group_items items) ~f:(function
      | Item item -> signature_item item
      | Type (rec_flag, type_decls) -> Ast_helper.Sig.type_ rec_flag type_decls))

and group_items (items : Types.signature_item list) =
  let rec read_type type_acc items =
    match items with
    | Sig_type (id, type_decl, Trec_next, _) :: rest ->
      let td = type_declaration id type_decl in
      read_type (td :: type_acc) rest
    | _ -> (List.rev type_acc, items)
  in
  let rec group acc items =
    match items with
    | Sig_type (id, type_decl, Trec_first, _) :: rest ->
      let type_, rest = read_type [ type_declaration id type_decl ] rest in
      group (Type (Asttypes.Recursive, type_) :: acc) rest
    | Sig_type (id, type_decl, Trec_not, _) :: rest ->
      let type_, rest = read_type [ type_declaration id type_decl ] rest in
      group (Type (Asttypes.Nonrecursive, type_) :: acc) rest
    | (Sig_class _ as item) :: _ :: _ :: rest -> group (Item item :: acc) rest
    | (Sig_class_type _ as item) :: _ :: rest -> group (Item item :: acc) rest
    | item :: rest -> group (Item item :: acc) rest
    | [] -> List.rev acc
  in
  group [] items
