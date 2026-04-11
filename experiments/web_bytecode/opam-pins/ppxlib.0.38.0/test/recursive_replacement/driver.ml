open Ppxlib

let payload : unit -> (structure_item, label list -> 'a, 'a) Ast_pattern.t =
 fun () ->
  Ast_pattern.(
    pstr_eval (elist (pexp_constant (pconst_string __ drop drop))) drop)

let make_payload ~loc labels =
  let open Ast_builder.Make (struct
    let loc = loc
  end) in
  pstr_eval (elist (List.map estring labels)) []

let expand ~loc ident recurse =
  let open Ast_builder.Make (struct
    let loc = loc
  end) in
  let ident =
    Located.mk
      (match ident with
      | Lident x -> Lident (x ^ "x")
      | _ -> Location.raise_errorf ~loc "ident must be simple")
  in
  match recurse with
  | [] -> pexp_ident ident
  | "%ext" :: recurse ->
      pexp_extension
        ( Located.mk "ext",
          PStr [ pstr_eval (pexp_ident ident) []; make_payload ~loc recurse ] )
  | "@attr" :: recurse ->
      {
        pexp_desc = Pexp_ident ident;
        pexp_attributes =
          [
            attribute ~name:(Located.mk "attr")
              ~payload:(PStr [ make_payload ~loc recurse ]);
          ];
        pexp_loc = loc;
        pexp_loc_stack = [];
      }
  | hd :: _ -> Location.raise_errorf ~loc "invalid rewrite: %s" hd

let () =
  Driver.register_transformation "recursive"
    ~rules:
      [
        Context_free.Rule.extension
          (Extension.V3.declare "ext" Extension.Context.expression
             Ast_pattern.(
               pstr (pstr_eval (pexp_ident __) drop ^:: payload () ^:: nil))
             (fun ~ctxt ident recurse ->
               let loc = Expansion_context.Extension.extension_point_loc ctxt in
               let loc = { loc with loc_ghost = true } in
               expand ~loc ident recurse));
        Context_free.Rule.attr_replace "attr" Extension.Context.expression
          (Attribute.declare "attr" Attribute.Context.Expression
             Ast_pattern.(pstr (payload () ^:: nil))
             (fun x -> x))
          (fun ~ctxt:_ x recurse ->
            match x.pexp_desc with
            | Pexp_ident ident -> expand ~loc:x.pexp_loc ident.txt recurse
            | _ ->
                Location.raise_errorf ~loc:x.pexp_loc
                  "rewrite must be applied to an identifier");
      ]

let () = Driver.standalone ()
