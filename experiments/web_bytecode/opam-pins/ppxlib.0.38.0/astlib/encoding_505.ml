open Stdlib0

module Ext_name = struct
  let pexp_struct_item = "ppxlib.migration.pexp_struct_item_5_5"
  let ptyp_functor = "ppxlib.migration.ptyp_functor_5_5"
  let preserve_ppat_constraint = "ppxlib.migration.preserve_ppat_constraint_5_5"
  let ptype_kind_external = "ppxlib.migration.ptype_kind_external_5_5"
  let external_psig = "ppxlib.migration.external_psig_5_5"
  let external_pstr_type = "ppxlib.migration.external_pstr_type_5_5"
  let external_pmty_with = "ppxlib.migration.external_pmty_with_5_5"
end

let invalid_encoding ~loc name =
  Location.raise_errorf ~loc "Invalid %s encoding" name

module To_504 = struct
  open Ast_504.Asttypes
  open Ast_504.Parsetree

  let encode_pexp_struct_item ~loc ((si, e) : structure_item * expression) =
    let expr_as_structure_item =
      let pstr_desc = Pstr_eval (e, []) in
      { pstr_desc; pstr_loc = loc }
    in
    let payload =
      let items = [ si; expr_as_structure_item ] in
      PStr items
    in
    let name = { txt = Ext_name.pexp_struct_item; loc } in
    Pexp_extension (name, payload)

  let decode_pexp_struct_item ~loc payload =
    match payload with
    | PStr [ si; { pstr_desc = Pstr_eval (e, []); _ } ] -> (si, e)
    | _ -> invalid_encoding ~loc Ext_name.pexp_struct_item

  let mk_core_type ?(ptyp_attributes = []) ptyp_desc =
    {
      ptyp_desc;
      ptyp_loc = Location.none;
      ptyp_loc_stack = [];
      ptyp_attributes;
    }

  let encode_ptyp_functor ~loc
      ((arg, name, pkg, typ) :
        arg_label * string loc * package_type * core_type) =
    let as_type =
      let pkg = mk_core_type (Ptyp_package pkg) in
      let attr = { attr_name = name; attr_payload = PStr []; attr_loc = loc } in
      mk_core_type ~ptyp_attributes:[ attr ] (Ptyp_arrow (arg, pkg, typ))
    in
    let payload = PTyp as_type in
    let name = { txt = Ext_name.ptyp_functor; loc } in
    Ptyp_extension (name, payload)

  let decode_ptyp_functor ~loc payload =
    match payload with
    | PTyp
        {
          ptyp_desc = Ptyp_arrow (arg, { ptyp_desc = Ptyp_package pkg }, typ);
          ptyp_attributes = [ attr ];
        } ->
        (arg, attr.attr_name, pkg, typ)
    | _ -> invalid_encoding ~loc Ext_name.ptyp_functor

  let encode_ptype_kind_external ~loc name attributes =
    let loc = { loc with Location.loc_ghost = true } in
    let name_attr =
      {
        attr_name = { txt = name; loc };
        attr_loc = loc;
        attr_payload = PStr [];
      }
    in
    let si = { pstr_desc = Pstr_attribute name_attr; pstr_loc = loc } in
    let flag_attr =
      {
        attr_name = { txt = Ext_name.ptype_kind_external; loc };
        attr_loc = loc;
        attr_payload = PStr [ si ];
      }
    in
    (Ptype_abstract, flag_attr :: attributes)

  let decode_ptype_kind_external type_decl =
    let attrs =
      List.without_first type_decl.ptype_attributes ~pred:(fun attr ->
          String.equal attr.attr_name.txt Ext_name.ptype_kind_external)
    in
    match (type_decl.ptype_kind, attrs) with
    | ( Ptype_abstract,
        Some
          ( {
              attr_name = { txt; _ };
              attr_payload =
                PStr
                  [
                    {
                      pstr_desc =
                        Pstr_attribute { attr_name = { txt = name; _ }; _ };
                    };
                  ];
            },
            ptype_attributes ) )
      when String.equal txt Ext_name.ptype_kind_external ->
        Some (name, ptype_attributes)
    | _ -> None

  let encode_external_psig ~loc psig_desc =
    let loc = { loc with Location.loc_ghost = true } in
    let ext =
      ( { txt = Ext_name.external_psig; loc },
        PSig [ { psig_loc = loc; psig_desc } ] )
    in
    Psig_extension (ext, [])

  let encode_external_psig_type ~loc rec_flag tds =
    encode_external_psig ~loc (Psig_type (rec_flag, tds))

  let encode_external_psig_typesubst ~loc tds =
    encode_external_psig ~loc (Psig_typesubst tds)

  let decode_external_psig ~loc payload attrs =
    match (payload, attrs) with
    | PSig [ { psig_desc = (Psig_type _ | Psig_typesubst _) as res; _ } ], [] ->
        res
    | _ -> invalid_encoding ~loc "external type signature_item_desc"

  let encode_external_pstr_type ~loc rec_flag tds =
    let loc = { loc with Location.loc_ghost = true } in
    let pstr_desc = Pstr_type (rec_flag, tds) in
    let ext =
      ( { txt = Ext_name.external_pstr_type; loc },
        PStr [ { pstr_loc = loc; pstr_desc } ] )
    in
    Pstr_extension (ext, [])

  let decode_external_pstr_type ~loc payload attrs =
    match (payload, attrs) with
    | PStr [ { pstr_desc = Pstr_type _ as res; _ } ], [] -> res
    | _ -> invalid_encoding ~loc "external type pstr_type"

  let encode_external_pmty_with ~loc mty constraints =
    let loc = { loc with Location.loc_ghost = true } in
    let pmd_type =
      {
        pmty_loc = loc;
        pmty_attributes = [];
        pmty_desc = Pmty_with (mty, constraints);
      }
    in
    let psig_desc =
      Psig_module
        {
          pmd_name = { txt = None; loc };
          pmd_type;
          pmd_attributes = [];
          pmd_loc = loc;
        }
    in
    let ext =
      ( { txt = Ext_name.external_pmty_with; loc },
        PSig [ { psig_loc = loc; psig_desc } ] )
    in
    Pmty_extension ext

  let decode_external_pmty_with ~loc payload =
    match payload with
    | PSig
        [
          {
            psig_desc =
              Psig_module
                {
                  pmd_name = { txt = None; _ };
                  pmd_attributes = [];
                  pmd_type =
                    { pmty_attributes = []; pmty_desc = Pmty_with _ as res; _ };
                  _;
                };
            _;
          };
        ] ->
        res
    | _ -> invalid_encoding ~loc "external type pmty_with"

  let must_preserve_ppat_constraint l =
    List.without_first l ~pred:(fun attr ->
        String.equal attr.attr_name.txt Ext_name.preserve_ppat_constraint)
    |> Option.map snd

  let preserve_ppat_constraint pattern core_type =
    let loc = pattern.ppat_loc in
    let flag =
      {
        attr_name = { txt = Ext_name.preserve_ppat_constraint; loc };
        attr_payload = PStr [];
        attr_loc = loc;
      }
    in
    Ppat_constraint
      ( { pattern with ppat_attributes = flag :: pattern.ppat_attributes },
        core_type )
end
