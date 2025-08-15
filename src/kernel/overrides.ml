open Std

let { Logger.log } = Logger.for_section "overrides"

let error_failed_to_parse_position_field_values =
  Error "failed to parse position field values"

let error_unexpected_position_expression_structure =
  Error "unexpected position expression structure"

let error_unexpected_payload_expression_structure =
  Error "unexpected payload expression structure"

let error_unexpected_merlin_override_attribute_structure =
  Error "unexpected merlin.X attribute structure"

module Attribute_name = struct
  type 'a t = Document : string t | Locate : Lexing.position t

  let to_name (type a) (t : a t) =
    match t with
    | Document -> "merlin.document"
    | Locate -> "merlin.locate"
end

module Override = struct
  let expr_to_pos ({ pexp_desc; _ } : Parsetree.expression) =
    match pexp_desc with
    | Pexp_record
        ( [ ( { txt = Lident "pos_fname"; _ },
              { pexp_desc = Pexp_constant (Pconst_string (pos_fname, _, _)); _ }
            );
            ( { txt = Lident "pos_lnum"; _ },
              { pexp_desc = Pexp_constant (Pconst_integer (lnum, None)); _ } );
            ( { txt = Lident "pos_bol"; _ },
              { pexp_desc = Pexp_constant (Pconst_integer (bol, None)); _ } );
            ( { txt = Lident "pos_cnum"; _ },
              { pexp_desc = Pexp_constant (Pconst_integer (cnum, None)); _ } )
          ],
          None ) -> (
      match
        (int_of_string_opt lnum, int_of_string_opt bol, int_of_string_opt cnum)
      with
      | Some pos_lnum, Some pos_bol, Some pos_cnum ->
        Ok { Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum }
      | _ -> error_failed_to_parse_position_field_values)
    | _ -> error_unexpected_position_expression_structure

  module Payload = struct
    type 'a t =
      | Document : string -> string t
      | Locate : Lexing.position -> Lexing.position t

    let of_expression (type a) ~(attribute_name : a Attribute_name.t)
        (expr : Parsetree.expression) : (a t, string) result =
      match (attribute_name, expr.pexp_desc) with
      | Document, Pexp_constant (Pconst_string (documentation, _, _)) ->
        Ok (Document documentation)
      | Locate, Pexp_record _ ->
        Result.map (expr_to_pos expr) ~f:(fun pos -> Locate pos)
      | _ -> error_unexpected_payload_expression_structure
  end

  type 'a t = { loc : Location.t; payload : 'a Payload.t }

  let of_expression ~attribute_name ({ pexp_desc; _ } : Parsetree.expression) =
    match pexp_desc with
    | Pexp_record
        ( [ ( { txt = Lident "location"; _ },
              { pexp_desc =
                  Pexp_record
                    ( [ ({ txt = Lident "loc_start"; _ }, loc_start_expr);
                        ({ txt = Lident "loc_end"; _ }, loc_end_expr);
                        ({ txt = Lident "loc_ghost"; _ }, loc_ghost_expr)
                      ],
                      None );
                _
              } );
            ({ txt = Lident "payload"; _ }, payload_expression)
          ],
          None ) ->
      let open Misc_stdlib.Monad.Result.Syntax in
      let* loc_start = expr_to_pos loc_start_expr in
      let* loc_end = expr_to_pos loc_end_expr in
      let* loc_ghost =
        match loc_ghost_expr.pexp_desc with
        | Pexp_construct ({ txt = Lident "false"; _ }, None) -> Ok false
        | Pexp_construct ({ txt = Lident "true"; _ }, None) -> Ok true
        | _ -> error_failed_to_parse_position_field_values
      in
      let* payload = Payload.of_expression ~attribute_name payload_expression in
      Ok { loc = { Location.loc_start; loc_end; loc_ghost }; payload }
    | _ -> error_unexpected_merlin_override_attribute_structure

  let payload (type a) (t : a t) : a =
    match t.payload with
    | Document doc -> doc
    | Locate loc -> loc

  let to_interval t =
    Overrides_interval_tree.Interval.create ~loc:t.loc ~payload:t
end

type 'a t = 'a Override.t Overrides_interval_tree.t

let rec of_payload ~attribute_name ({ pexp_desc; _ } : Parsetree.expression) =
  match pexp_desc with
  | Pexp_construct
      ( { txt = Lident "::"; _ },
        Some { pexp_desc = Pexp_tuple [ (None, override); (None, rest) ]; _ } )
    -> (
    match Override.of_expression ~attribute_name override with
    | Ok override -> override :: of_payload ~attribute_name rest
    | Error err ->
      log ~title:"of_payload" "%s" err;
      of_payload ~attribute_name rest)
  | _ -> []

let of_attribute ~attribute_name (attribute : Parsetree.attribute) =
  match attribute with
  | { attr_payload = PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ]; _ } ->
    Ok (of_payload ~attribute_name expr)
  | _ -> error_unexpected_merlin_override_attribute_structure

let get_overrides ~attribute_name (ppx_parsetree : Mreader.parsetree) =
  let attributes =
    match ppx_parsetree with
    | `Interface signature ->
      List.filter_map signature.psg_items
        ~f:(fun (signature_item : Parsetree.signature_item) ->
          match signature_item.psig_desc with
          | Psig_attribute ({ attr_name = { txt; _ }; _ } as attr)
            when String.equal (Attribute_name.to_name attribute_name) txt ->
            Some attr
          | _ -> None)
    | `Implementation structure ->
      List.filter_map structure
        ~f:(fun (structure_item : Parsetree.structure_item) ->
          match structure_item.pstr_desc with
          | Pstr_attribute ({ attr_name = { txt; _ }; _ } as attr)
            when String.equal (Attribute_name.to_name attribute_name) txt ->
            Some attr
          | _ -> None)
  in
  attributes
  |> List.concat_map ~f:(fun attribute ->
         match of_attribute ~attribute_name attribute with
         | Ok overrides -> overrides
         | Error err ->
           log ~title:"get_overrides" "%s" err;
           [])
  |> List.filter_map ~f:(fun (override : _ Override.t) ->
         match Override.to_interval override with
         | Ok interval -> Some interval
         | Error err ->
           log ~title:"get_overrides" "%s" err;
           None)
  |> Overrides_interval_tree.of_alist

let find t ~cursor = Overrides_interval_tree.find t cursor
