open Ppxlib
open Ast_builder.Default

(*
   [[@@deriving foo]] expands to:
   {[
     module Foo = struct end

     let _ = (); (); [%foo]
   ]}

   and then [[%foo]] expands to ["foo"].
*)

let add_deriver () =
  let str_type_decl =
    Deriving.Generator.make_noarg
      (fun ~loc ~path:_ _ ->
        let expr desc : expression =
          {
            pexp_desc = desc;
            pexp_loc = loc;
            pexp_attributes = [];
            pexp_loc_stack = [];
          }
        in
        [
          {
            pstr_loc = loc;
            pstr_desc =
              Pstr_module
                {
                  pmb_loc = loc;
                  pmb_name = { loc; txt = Some "Foo" };
                  pmb_expr =
                    {
                      pmod_loc = loc;
                      pmod_desc = Pmod_structure [];
                      pmod_attributes = [];
                    };
                  pmb_attributes = [];
                };
          };
          {
            pstr_loc = loc;
            pstr_desc =
              Pstr_value
                ( Nonrecursive,
                  [
                    {
                      pvb_pat =
                        {
                          ppat_desc = Ppat_any;
                          ppat_loc = loc;
                          ppat_attributes = [];
                          ppat_loc_stack = [];
                        };
                      pvb_expr =
                        esequence ~loc
                          [
                            eunit ~loc;
                            eunit ~loc;
                            expr
                              (Pexp_extension ({ loc; txt = "foo" }, PStr []));
                          ];
                      pvb_attributes = [];
                      pvb_loc = loc;
                      pvb_constraint = None;
                      pvb_is_poly = false;
                      pvb_modes = [];
                    };

                  ] );
          };
        ])
      ~attributes:[]
  in
  let sig_type_decl =
    Deriving.Generator.make_noarg (fun ~loc ~path decl ->
        ignore loc;
        ignore path;
        ignore decl;
        [])
  in
  Deriving.add "foo" ~str_type_decl ~sig_type_decl

let () =
  Driver.register_transformation "foo"
    ~rules:
      [
        Context_free.Rule.extension
          (Extension.declare "foo" Expression Ast_pattern.__
             (fun ~loc ~path:_ _payload ->
               {
                 pexp_desc = Pexp_constant (Pconst_string ("foo", loc, None));
                 pexp_loc = loc;
                 pexp_attributes = [];
                 pexp_loc_stack = [];
               }));
      ]

let (_ : Deriving.t) = add_deriver ()

(* [[@@deriving nested_jkind]] expands to [type nested : value non_null non_float],
   built as two nested [Pjk_operator]s, as a ppx substituting [value non_null] for
   [k] in [k non_float] would. The parser reads it back as a single one. *)
let (_ : Deriving.t) =
  let str_type_decl =
    Deriving.Generator.make_noarg (fun ~loc ~path:_ _ ->
        let jkind pjka_desc = { pjka_loc = loc; pjka_desc } in
        let value = jkind (Pjk_abbreviation { loc; txt = Lident "value" }) in
        let inner = jkind (Pjk_operator (value, [ { loc; txt = "non_null" } ])) in
        let outer = jkind (Pjk_operator (inner, [ { loc; txt = "non_float" } ])) in
        let td =
          type_declaration ~loc ~name:{ loc; txt = "nested" } ~params:[]
            ~cstrs:[] ~kind:Ptype_abstract ~private_:Public ~manifest:None
        in
        [
          pstr_type ~loc Recursive
            [ { td with ptype_jkind_annotation = Some outer } ];
        ])
  in
  Deriving.add "nested_jkind" ~str_type_decl
