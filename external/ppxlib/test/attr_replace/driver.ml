open Ppxlib

let string_pattern = Ast_pattern.(single_expr_payload (estring __))

let template_class_expr ~ctxt:_ class_expr payload =
  match class_expr.pcl_desc with
  | Pcl_constr ({ txt = Lident name; loc }, args) ->
      {
        class_expr with
        pcl_desc =
          Pcl_constr ({ txt = Lident (name ^ "__" ^ payload); loc }, args);
      }
  | _ -> class_expr

let () =
  Driver.register_transformation "test.clx"
    ~rules:
      [
        Context_free.Rule.attr_replace "test.clx" Extension.Context.class_expr
          (Attribute.declare "test.clx" Class_expr string_pattern Fun.id)
          template_class_expr;
      ]

let template_class_field ~ctxt:_ class_field payload =
  match class_field.pcf_desc with
  | Pcf_val ({ txt = name; loc }, flag, kind) ->
      {
        class_field with
        pcf_desc = Pcf_val ({ txt = name ^ "__" ^ payload; loc }, flag, kind);
      }
  | _ -> class_field

let () =
  Driver.register_transformation "test.clf"
    ~rules:
      [
        Context_free.Rule.attr_replace "test.clf" Extension.Context.class_field
          (Attribute.declare "test.clf" Class_field string_pattern Fun.id)
          template_class_field;
      ]

let template_class_type ~ctxt:_ class_type payload =
  match class_type.pcty_desc with
  | Pcty_constr ({ txt = Lident name; loc }, args) ->
      {
        class_type with
        pcty_desc =
          Pcty_constr ({ txt = Lident (name ^ "__" ^ payload); loc }, args);
      }
  | _ -> class_type

let () =
  Driver.register_transformation "test.clt"
    ~rules:
      [
        Context_free.Rule.attr_replace "test.clt" Extension.Context.class_type
          (Attribute.declare "test.clt" Class_type string_pattern Fun.id)
          template_class_type;
      ]

let template_class_type_field ~ctxt:_ class_type_field payload =
  match class_type_field.pctf_desc with
  | Pctf_val ({ txt = name; loc }, mut_flag, virt_flag, ty) ->
      {
        class_type_field with
        pctf_desc =
          Pctf_val
            ({ txt = name ^ "__" ^ payload; loc }, mut_flag, virt_flag, ty);
      }
  | _ -> class_type_field

let () =
  Driver.register_transformation "test.ctf"
    ~rules:
      [
        Context_free.Rule.attr_replace "test.ctf"
          Extension.Context.class_type_field
          (Attribute.declare "test.ctf" Class_type_field string_pattern Fun.id)
          template_class_type_field;
      ]

let template_core_type ~ctxt:_ core_type payload =
  match core_type.ptyp_desc with
  | Ptyp_constr ({ txt = Lident name; loc }, args) ->
      {
        core_type with
        ptyp_desc =
          Ptyp_constr ({ txt = Lident (name ^ "__" ^ payload); loc }, args);
      }
  | _ -> core_type

let () =
  Driver.register_transformation "test.typ"
    ~rules:
      [
        Context_free.Rule.attr_replace "test.typ" Extension.Context.core_type
          (Attribute.declare "test.typ" Core_type string_pattern Fun.id)
          template_core_type;
      ]

let template_expression ~ctxt:_ expression payload =
  match expression.pexp_desc with
  | Pexp_ident { txt = Lident name; loc } ->
      {
        expression with
        pexp_desc = Pexp_ident { txt = Lident (name ^ "__" ^ payload); loc };
      }
  | _ -> expression

let () =
  Driver.register_transformation "test.exp"
    ~rules:
      [
        Context_free.Rule.attr_replace "test.exp" Extension.Context.expression
          (Attribute.declare "test.exp" Expression string_pattern Fun.id)
          template_expression;
      ]

let template_module_expr ~ctxt:_ module_expr payload =
  match module_expr.pmod_desc with
  | Pmod_ident { txt = Lident name; loc } ->
      {
        module_expr with
        pmod_desc = Pmod_ident { txt = Lident (name ^ "__" ^ payload); loc };
      }
  | _ -> module_expr

let () =
  Driver.register_transformation "test.mod_exp"
    ~rules:
      [
        Context_free.Rule.attr_replace "test.mod_exp"
          Extension.Context.module_expr
          (Attribute.declare "test.mod_exp" Module_expr string_pattern Fun.id)
          template_module_expr;
      ]

let template_module_type ~ctxt:_ module_type payload =
  match module_type.pmty_desc with
  | Pmty_ident { txt = Lident name; loc } ->
      {
        module_type with
        pmty_desc = Pmty_ident { txt = Lident (name ^ "__" ^ payload); loc };
      }
  | _ -> module_type

let () =
  Driver.register_transformation "test.mod_typ"
    ~rules:
      [
        Context_free.Rule.attr_replace "test.mod_typ"
          Extension.Context.module_type
          (Attribute.declare "test.mod_typ" Module_type string_pattern Fun.id)
          template_module_type;
      ]

let template_pattern ~ctxt:_ pattern payload =
  match pattern.ppat_desc with
  | Ppat_var { txt = name; loc } ->
      { pattern with ppat_desc = Ppat_var { txt = name ^ "__" ^ payload; loc } }
  | _ -> pattern

let () =
  Driver.register_transformation "test.pat"
    ~rules:
      [
        Context_free.Rule.attr_replace "test.pat" Extension.Context.pattern
          (Attribute.declare "test.pat" Pattern string_pattern Fun.id)
          template_pattern;
      ]

let template_sig_extension ~ctxt:_ sig_item payload =
  match sig_item.psig_desc with
  | Psig_extension ((ext, inner_payload), attrs) ->
      {
        sig_item with
        psig_desc =
          Psig_extension
            (({ ext with txt = ext.txt ^ "__" ^ payload }, inner_payload), attrs);
      }
  | _ -> assert false

let () =
  Driver.register_transformation "test.sig.ext"
    ~rules:
      [
        Context_free.Rule.attr_replace "test.sig.ext"
          Extension.Context.signature_item
          (Attribute.declare "test.sig.ext" Psig_extension string_pattern Fun.id)
          template_sig_extension;
      ]

let template_str_extension ~ctxt:_ structure_item payload =
  match structure_item.pstr_desc with
  | Pstr_extension ((ext, inner_payload), attrs) ->
      {
        structure_item with
        pstr_desc =
          Pstr_extension
            (({ ext with txt = ext.txt ^ "__" ^ payload }, inner_payload), attrs);
      }
  | _ -> assert false

let () =
  Driver.register_transformation "test.str.ext"
    ~rules:
      [
        Context_free.Rule.attr_replace "test.str.ext"
          Extension.Context.structure_item
          (Attribute.declare "test.str.ext" Pstr_extension string_pattern Fun.id)
          template_str_extension;
      ]

let template_str_eval ~ctxt:_ structure_item payload =
  match structure_item.pstr_desc with
  | Pstr_eval (expression, attributes) ->
      let expression =
        match expression.pexp_desc with
        | Pexp_ident { txt = Lident name; loc } ->
            {
              expression with
              pexp_desc =
                Pexp_ident { txt = Lident (name ^ "__" ^ payload); loc };
            }
        | _ -> expression
      in
      { structure_item with pstr_desc = Pstr_eval (expression, attributes) }
  | _ -> assert false

let () =
  Driver.register_transformation "test.str.evl"
    ~rules:
      [
        Context_free.Rule.attr_replace "test.str.evl"
          Extension.Context.structure_item
          (Attribute.declare "test.str.evl" Pstr_eval string_pattern Fun.id)
          template_str_eval;
      ]

let template_ppx_import ~ctxt:_ _payload = assert false

let () =
  Driver.register_transformation "test.ppx.import"
    ~rules:
      [
        Context_free.Rule.attr_replace "test.ppx.import"
          Extension.Context.Ppx_import
          (Attribute.declare "test.ppx.import" Type_declaration string_pattern
             Fun.id)
          template_ppx_import;
      ]

let attr_multi ~ctxt:_ expression
    ([ prefix; suffix ] :
      _ Context_free.Rule.Attr_multiple_replace.Parsed_payload_list.t) =
  match (prefix, suffix) with
  | None, None -> assert false
  | _ -> (
      ();
      match expression.pexp_desc with
      | Pexp_ident { txt = Lident name; loc } ->
          let prefixed = Option.value ~default:"" prefix ^ name in
          let suffixed = prefixed ^ Option.value ~default:"" suffix in
          {
            expression with
            pexp_desc = Pexp_ident { txt = Lident suffixed; loc };
          }
      | _ -> expression)

let () =
  Driver.register_transformation "test"
    ~rules:
      [
        Context_free.Rule.Attr_multiple_replace.attr_multiple_replace
          "test.multi.exp" Extension.Context.expression
          [
            Attribute.declare "test.multi.exp.prefix" Expression string_pattern
              Fun.id;
            Attribute.declare "test.multi.exp.suffix" Expression string_pattern
              Fun.id;
          ]
          attr_multi;
      ]

let () = Driver.standalone ()
