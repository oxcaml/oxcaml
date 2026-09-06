open Types
module Modality = Mode.Modality.Const

type layer =
  { wrapper : type_expr;
    payload : type_expr;
    modality : Modality.t
  }

type plan =
  | Equal
  | Introduce of layer
  | Eliminate of layer

type head =
  | Unknown
  | Wrapped
  | Bare

let classify ty =
  match get_desc ty with Tvar _ -> Unknown | Tmod _ -> Wrapped | _ -> Bare

let head env ty =
  (* Looking ahead must not mark a shared expected type as dependent on a
     GADT equation. Only the classification escapes this temporary expansion. *)
  let snapshot = Btype.snapshot () in
  Fun.protect
    (fun () ->
      let principal = Ctype.is_principal ty in
      let ty = Ctype.expand_head env ty in
      classify ty, principal && Ctype.is_principal ty)
    ~finally:(fun () -> Btype.backtrack snapshot)

let layer env ty =
  match get_desc (Ctype.expand_head env ty) with
  | Tmod (payload, modality) -> { wrapper = ty; payload; modality }
  | _ -> Misc.fatal_error "Modality_elaboration.layer: expected a wrapper"

let check_principal loc principal =
  if not principal
  then
    Location.prerr_warning loc
      (Warnings.Not_principal
         (Format_doc.doc_printf "this implicit modality conversion"))

let outer_layer ?loc env ty =
  match head env ty with
  | Wrapped, principal ->
    Option.iter (fun loc -> check_principal loc principal) loc;
    Some (layer env ty)
  | (Unknown | Bare), _ -> None

let plan ~loc env ~actual ~expected =
  match head env actual, head env expected with
  | (Unknown, _), _
  | _, (Unknown, _)
  | (Wrapped, _), (Wrapped, _)
  | (Bare, _), (Bare, _) ->
    Equal
  | (Bare, p), (Wrapped, q) ->
    check_principal loc (p && q);
    Introduce (layer env expected)
  | (Wrapped, p), (Bare, q) ->
    check_principal loc (p && q);
    Eliminate (layer env actual)

let introduction ~loc env ~expected (desc : Parsetree.expression_desc) =
  let produces_head =
    match desc with
    | Parsetree.Pexp_constant _ | Pexp_function _ | Pexp_unboxed_unit
    | Pexp_unboxed_bool _ | Pexp_tuple _ | Pexp_unboxed_tuple _
    | Pexp_construct _ | Pexp_variant _ | Pexp_record _
    | Pexp_record_unboxed_product _ | Pexp_array _ | Pexp_comprehension _
    | Pexp_idx _ | Pexp_setfield _ | Pexp_setvar _ | Pexp_while _ | Pexp_for _
    | Pexp_lazy _ | Pexp_object _ | Pexp_new _ | Pexp_pack _ | Pexp_quote _
    | Pexp_stack _ | Pexp_extension _ ->
      true
    | _ -> false
  in
  if produces_head then outer_layer ~loc env expected else None

let pattern_elimination ~loc env ~expected (desc : Parsetree.pattern_desc) =
  let inspects_head =
    match desc with
    | Parsetree.Ppat_constant _ | Ppat_interval _ | Ppat_unboxed_unit
    | Ppat_unboxed_bool _ | Ppat_tuple _ | Ppat_unboxed_tuple _
    | Ppat_construct _ | Ppat_variant _ | Ppat_record _
    | Ppat_record_unboxed_product _ | Ppat_array _ | Ppat_type _ | Ppat_lazy _
    | Ppat_unpack _ ->
      true
    | _ -> false
  in
  if inspects_head then outer_layer ~loc env expected else None

let expression ~ty (child : Typedtree.expression) =
  { child with
    exp_desc = Typedtree.Texp_modality child;
    exp_type = ty;
    exp_extra = [];
    exp_attributes = []
  }

let pattern ~ty (child : Typedtree.pattern) =
  { child with
    pat_desc = Typedtree.Tpat_modality child;
    pat_type = ty;
    pat_extra = [];
    pat_attributes = []
  }
