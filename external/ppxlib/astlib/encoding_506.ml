open Stdlib0

module Ext_name = struct
  let pstr_primitive_alias = "ppxlib.migration.pstr_primitive_alias_5_6"
  let psig_primitive_alias = "ppxlib.migration.psig_primitive_alias_5_6"
  let none = "ppxlib.migration.none_5_6"
  let primitive_alias = "ppxlib.migration.primitive_alias_5_6"
end

let invalid_encoding ~loc name = Error.invalid_encoding ~loc ~version:"5.6" name

module To_505 = struct
  open Ast_505.Asttypes
  open Ast_505.Parsetree

  let encode_typ_opt ~loc typ_opt =
    match typ_opt with
    | Some typ -> typ
    | None ->
        let ptyp_desc =
          Ptyp_extension ({ txt = Ext_name.none; loc }, PStr [])
        in
        { ptyp_desc; ptyp_loc = loc; ptyp_attributes = []; ptyp_loc_stack = [] }

  let decode_typ_opt ~loc core_type =
    match core_type.ptyp_desc with
    | Ptyp_extension ({ txt; _ }, payload) when String.equal txt Ext_name.none
      -> (
        match payload with
        | PStr [] -> None
        | _ -> invalid_encoding ~loc Ext_name.none)
    | _ -> Some core_type

  let encode_alias ~loc lident_loc =
    let attr_name = { txt = Ext_name.primitive_alias; loc } in
    let ident_expr =
      let pexp_desc = Pexp_ident lident_loc in
      { pexp_desc; pexp_loc = loc; pexp_attributes = []; pexp_loc_stack = [] }
    in
    let ident_stri =
      let pstr_desc = Pstr_eval (ident_expr, []) in
      { pstr_desc; pstr_loc = loc }
    in
    let attr_payload = PStr [ ident_stri ] in
    { attr_name; attr_payload; attr_loc = loc }

  let decode_alias ~loc attr_payload =
    match attr_payload with
    | PStr
        [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_ident ident; _ }, []) } ]
      ->
        ident
    | _ -> invalid_encoding ~loc Ext_name.primitive_alias

  let encode_primitive_alias ~loc pval_name typ_opt ident attrs =
    let pval_type = encode_typ_opt ~loc typ_opt in
    let alias_attr = encode_alias ~loc ident in
    let pval_attributes = alias_attr :: attrs in
    let vd =
      { pval_name; pval_type; pval_attributes; pval_loc = loc; pval_prim = [] }
    in
    let stri = { pstr_desc = Pstr_primitive vd; pstr_loc = loc } in
    PStr [ stri ]

  let decode_primitive_alias ~loc ~name payload =
    match payload with
    | PStr [ { pstr_desc = Pstr_primitive vd; _ } ] -> (
        let alias_attr_and_remainder =
          List.without_first vd.pval_attributes ~pred:(fun a ->
              String.equal a.attr_name.txt Ext_name.primitive_alias)
        in
        match alias_attr_and_remainder with
        | None -> invalid_encoding ~loc name
        | Some (alias_attr, remainder_attrs) ->
            let lident_loc = decode_alias ~loc alias_attr.attr_payload in
            let typ_opt = decode_typ_opt ~loc vd.pval_type in
            (vd.pval_name, typ_opt, lident_loc, remainder_attrs))
    | _ -> invalid_encoding ~loc name

  let encode_psig_primitive_alias ~loc pval_name typ_opt ident attrs =
    let payload = encode_primitive_alias ~loc pval_name typ_opt ident attrs in
    Psig_extension (({ txt = Ext_name.psig_primitive_alias; loc }, payload), [])

  let decode_psig_primitive_alias ~loc payload attrs =
    match attrs with
    | [] ->
        decode_primitive_alias ~loc ~name:Ext_name.psig_primitive_alias payload
    | _ -> invalid_encoding ~loc Ext_name.psig_primitive_alias

  let encode_pstr_primitive_alias ~loc pval_name typ_opt ident attrs =
    let payload = encode_primitive_alias ~loc pval_name typ_opt ident attrs in
    Pstr_extension (({ txt = Ext_name.pstr_primitive_alias; loc }, payload), [])

  let decode_pstr_primitive_alias ~loc payload attrs =
    match attrs with
    | [] ->
        decode_primitive_alias ~loc ~name:Ext_name.pstr_primitive_alias payload
    | _ -> invalid_encoding ~loc Ext_name.psig_primitive_alias
end
