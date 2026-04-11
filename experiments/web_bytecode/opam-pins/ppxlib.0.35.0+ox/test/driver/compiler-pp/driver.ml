open Ppxlib

let existential ~loc =
  let lident = { loc; txt = Longident.parse "Constructor" } in
  let pattern =
    {
      ppat_loc = loc;
      ppat_loc_stack = [];
      ppat_attributes = [];
      ppat_desc =
        Ppat_construct (lident, Some ([ { loc; txt = "a" } ], [%pat? _]));
    }
  in
  [%stri let f x = match x with [%p pattern] -> ()]

let named_existential =
  Context_free.Rule.extension
    (Extension.V3.declare "named_existentials" Extension.Context.structure_item
       Ast_pattern.(pstr nil)
       (fun ~ctxt ->
         let loc = Expansion_context.Extension.extension_point_loc ctxt in
         existential ~loc))

let () =
  Driver.V2.register_transformation ~rules:[ named_existential ]
    "named_existentials"

let str_type_decl =
  Deriving.Generator.V2.make_noarg (fun ~ctxt _type_decl ->
      let loc = Expansion_context.Deriver.derived_item_loc ctxt in
      [ existential ~loc ])

let _ = Deriving.add ~str_type_decl "named_existentials"
let () = Driver.standalone ()
