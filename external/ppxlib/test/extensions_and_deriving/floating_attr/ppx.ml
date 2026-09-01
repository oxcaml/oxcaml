open Ppxlib

let () =
  let open Context_free.Rule in
  let open Ast_pattern in
  let ghost =
    object
      inherit Ast_traverse.map
      method! location loc = { loc with loc_ghost = true }
    end
  in
  let make_str_attr name =
    Attribute.Floating.declare name Structure_item (pstr __) (fun x -> x)
  in
  let make_sig_attr name =
    Attribute.Floating.declare name Signature_item (psig __) (fun x -> x)
  in
  let expand_str ~ctxt:_ x = ghost#structure x in
  let expand_sig ~ctxt:_ x = ghost#signature x in
  let make_str name rule = rule (make_str_attr name) expand_str in
  let make_sig name rule = rule (make_sig_attr name) expand_sig in
  let rules =
    [
      make_str "identity_inline_expanded" attr_str_floating_expect_and_expand;
      make_sig "identity_inline_expanded" attr_sig_floating_expect_and_expand;
    ]
  in
  Driver.register_transformation "identity" ~rules

let () =
  let extractor = Ast_pattern.(single_expr_payload (estring __)) in
  let expander ~loc ~path:_ s =
    Ast_builder.Default.(estring ~loc (s ^ "_suffix"))
  in
  let extender =
    Extension.declare "suffix" Extension.Context.Expression extractor expander
  in
  Driver.register_transformation "suffix"
    ~rules:[ Context_free.Rule.extension extender ]

let () =
  let extractor = Ast_pattern.(pstr nil) in
  let expander ~loc ~path:_ = [%type: string] in
  let extender =
    Extension.declare "str" Extension.Context.Core_type extractor expander
  in
  Driver.register_transformation "str"
    ~rules:[ Context_free.Rule.extension extender ]

let () = Driver.standalone ()
