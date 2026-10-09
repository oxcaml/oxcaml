open Ppxlib
open Ast_builder.Default

(* [[%rebuild_letop let* x = e in body]] rebuilds the [let*] from scratch, giving
   the binding operator the location of the whole expression, as the parser
   does. *)
let rebuild_letop =
  Extension.V3.declare "rebuild_letop" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~ctxt:_ e ->
      match e.pexp_desc with
      | Pexp_letop { let_; ands = []; body } ->
          let loc = e.pexp_loc in
          let let_ =
            binding_op ~loc ~op:let_.pbop_op ~pat:let_.pbop_pat
              ~exp:let_.pbop_exp
          in
          pexp_letop ~loc (letop ~let_ ~ands:[] ~body)
      | _ -> Location.raise_errorf ~loc:e.pexp_loc "expected a let*")

(* [[%%bad_jkind_loc]] generates [type bad : value] where the jkind annotation's
   location extends past the end of the type declaration's, while its [value]
   ident stays inside. *)
let bad_jkind_loc =
  Extension.V3.declare "bad_jkind_loc" Extension.Context.structure_item
    Ast_pattern.(pstr nil)
    (fun ~ctxt ->
      let loc = Expansion_context.Extension.extension_point_loc ctxt in
      let too_wide =
        {
          loc with
          loc_end = { loc.loc_end with pos_cnum = loc.loc_end.pos_cnum + 5 };
        }
      in
      let jkind =
        {
          pjka_loc = too_wide;
          pjka_desc = Pjk_abbreviation { loc; txt = Lident "value" };
        }
      in
      let td =
        type_declaration ~loc ~name:{ loc; txt = "bad" } ~params:[] ~cstrs:[]
          ~kind:Ptype_abstract ~private_:Public ~manifest:None
      in
      pstr_type ~loc Recursive [ { td with ptype_jkind_annotation = Some jkind } ])

let () =
  Driver.register_transformation "location_check"
    ~rules:
      [
        Context_free.Rule.extension rebuild_letop;
        Context_free.Rule.extension bad_jkind_loc;
      ]

let () = Driver.standalone ()
