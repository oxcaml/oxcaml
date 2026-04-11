open Stdlib0

module Ext_name = struct
  let ptyp_labeled_tuple = "ppxlib.migration.ptyp_labeled_tuple_5_4"
  let pexp_labeled_tuple = "ppxlib.migration.pexp_labeled_tuple_5_4"
  let ppat_labeled_tuple = "ppxlib.migration.ppat_labeled_tuple_5_4"
  let bivariant_param = "ppxlib.migration.bivariant_param_5_4"
  let bivariant_pstr = "ppxlib.migration.bivariant_str_item_5_4"
  let bivariant_psig = "ppxlib.migration.bivariant_sig_item_5_4"
  let bivariant_pmty_with = "ppxlib.migration.bivariant_pmty_with_5_4"
end

let invalid_encoding ~loc name =
  Location.raise_errorf ~loc "Invalid %s encoding" name

module type AST = sig
  type payload
  type core_type
  type core_type_desc
  type expression
  type expression_desc
  type pattern
  type pattern_desc
  type closed_flag

  module Construct : sig
    val ptyp_extension_desc : string Location.loc -> payload -> core_type_desc
    val ptyp_tuple : loc:Location.t -> core_type list -> core_type
    val ptyp_var : loc:Location.t -> string -> core_type
    val ptyp_any : loc:Location.t -> core_type
    val ptyp : core_type -> payload
    val pexp_extension_desc : string Location.loc -> payload -> expression_desc
    val pexp_tuple : loc:Location.t -> expression list -> expression

    val pexp_variant :
      loc:Location.t -> string -> expression option -> expression

    val pstr_eval : loc:Location.t -> expression -> payload
    val ppat_extension_desc : string Location.loc -> payload -> pattern_desc
    val ppat_tuple : loc:Location.t -> pattern list -> pattern
    val ppat_var : loc:Location.t -> string -> pattern
    val ppat_any : loc:Location.t -> pattern
    val ppat : pattern -> payload
    val closed_flag_to_string : closed_flag -> string
  end

  module Destruct : sig
    val ptyp : payload -> core_type option
    val ptyp_tuple : core_type -> core_type list option
    val ptyp_var : core_type -> string option
    val ptyp_any : core_type -> unit option
    val pstr_eval : payload -> expression option
    val pexp_tuple : expression -> expression list option
    val pexp_variant : expression -> (string * expression option) option
    val ppat : payload -> pattern option
    val ppat_tuple : pattern -> pattern list option
    val ppat_var : pattern -> string option
    val ppat_any : pattern -> unit option
    val closed_flag_from_string : string -> closed_flag option
  end
end

module Make (X : AST) = struct
  let encode_ptyp_labeled_tuple ~loc args =
    let payload =
      let l =
        List.map
          (fun (label_opt, typ) ->
            let label =
              match label_opt with
              | None -> X.Construct.ptyp_any ~loc
              | Some s -> X.Construct.ptyp_var ~loc s
            in
            X.Construct.ptyp_tuple ~loc [ label; typ ])
          args
      in
      X.Construct.ptyp_tuple ~loc l
    in
    X.Construct.ptyp_extension_desc
      { txt = Ext_name.ptyp_labeled_tuple; loc }
      (X.Construct.ptyp payload)

  let decode_ptyp_labeled_tuple ~loc payload =
    let open Stdlib0.Option.Op in
    let res =
      let* typ = X.Destruct.ptyp payload in
      let* typ_list = X.Destruct.ptyp_tuple typ in
      Stdlib0.Option.List.map typ_list ~f:(fun typ ->
          let* typ_pair = X.Destruct.ptyp_tuple typ in
          match typ_pair with
          | [ label; typ ] -> (
              match (X.Destruct.ptyp_var label, X.Destruct.ptyp_any label) with
              | Some s, _ -> Some (Some s, typ)
              | _, Some () -> Some (None, typ)
              | None, None -> None)
          | _ -> None)
    in
    match res with
    | Some res -> res
    | None -> invalid_encoding ~loc Ext_name.ptyp_labeled_tuple

  let encode_pexp_labeled_tuple ~loc args =
    let payload =
      let l =
        List.map
          (fun (label_opt, expr) ->
            let label =
              match label_opt with
              | None -> X.Construct.pexp_variant ~loc "None" None
              | Some s ->
                  let string_as_variant =
                    X.Construct.pexp_variant ~loc s None
                  in
                  X.Construct.pexp_variant ~loc "Some" (Some string_as_variant)
            in
            X.Construct.pexp_tuple ~loc [ label; expr ])
          args
      in
      X.Construct.pexp_tuple ~loc l
    in
    X.Construct.pexp_extension_desc
      { txt = Ext_name.pexp_labeled_tuple; loc }
      (X.Construct.pstr_eval ~loc payload)

  let decode_pexp_labeled_tuple ~loc payload =
    let open Stdlib0.Option.Op in
    let res =
      let* exp = X.Destruct.pstr_eval payload in
      let* exp_list = X.Destruct.pexp_tuple exp in
      Stdlib0.Option.List.map exp_list ~f:(fun exp ->
          let* exp_pair = X.Destruct.pexp_tuple exp in
          match exp_pair with
          | [ label; exp ] -> (
              let* opt_variant = X.Destruct.pexp_variant label in
              match opt_variant with
              | "None", None -> Some (None, exp)
              | "Some", Some exp' -> (
                  let* label_variant = X.Destruct.pexp_variant exp' in
                  match label_variant with
                  | s, None -> Some (Some s, exp)
                  | _, _ -> None)
              | _ -> None)
          | _ -> None)
    in
    match res with
    | Some res -> res
    | None -> invalid_encoding ~loc Ext_name.pexp_labeled_tuple

  let encode_ppat_labeled_tuple ~loc pats closed_flag =
    let payload =
      let flag =
        let s = X.Construct.closed_flag_to_string closed_flag in
        X.Construct.ppat_var ~loc s
      in
      let pats =
        let l =
          List.map
            (fun (label_opt, pat) ->
              let label =
                match label_opt with
                | None -> X.Construct.ppat_any ~loc
                | Some s -> X.Construct.ppat_var ~loc s
              in
              X.Construct.ppat_tuple ~loc [ label; pat ])
            pats
        in
        X.Construct.ppat_tuple ~loc l
      in
      X.Construct.ppat_tuple ~loc [ pats; flag ]
    in
    X.Construct.ppat_extension_desc
      { txt = Ext_name.ppat_labeled_tuple; loc }
      (X.Construct.ppat payload)

  let decode_ppat_labeled_tuple ~loc payload =
    let open Stdlib0.Option.Op in
    let res =
      let* pat = X.Destruct.ppat payload in
      let* pats_and_flag = X.Destruct.ppat_tuple pat in
      match pats_and_flag with
      | [ pats; flag ] ->
          let* flag_s = X.Destruct.ppat_var flag in
          let* closed_flag = X.Destruct.closed_flag_from_string flag_s in
          let* pat_list = X.Destruct.ppat_tuple pats in
          let* pats =
            Stdlib0.Option.List.map pat_list ~f:(fun pat ->
                let* pat_pair = X.Destruct.ppat_tuple pat in
                match pat_pair with
                | [ label; pat ] -> (
                    match
                      (X.Destruct.ppat_var label, X.Destruct.ppat_any label)
                    with
                    | Some s, _ -> Some (Some s, pat)
                    | _, Some () -> Some (None, pat)
                    | None, None -> None)
                | _ -> None)
          in
          Some (pats, closed_flag)
      | _ -> None
    in
    match res with
    | Some res -> res
    | None -> invalid_encoding ~loc Ext_name.ppat_labeled_tuple
end

module Ast_503_arg = struct
  include Ast_503.Asttypes
  include Ast_503.Parsetree

  module Construct = struct
    let core_type ~loc ptyp_desc =
      { ptyp_desc; ptyp_loc = loc; ptyp_attributes = []; ptyp_loc_stack = [] }

    let expression ~loc pexp_desc =
      { pexp_desc; pexp_loc = loc; pexp_attributes = []; pexp_loc_stack = [] }

    let pattern ~loc ppat_desc =
      { ppat_desc; ppat_loc = loc; ppat_attributes = []; ppat_loc_stack = [] }

    let ptyp_extension_desc name payload = Ptyp_extension (name, payload)
    let ptyp_tuple ~loc typs = core_type ~loc (Ptyp_tuple typs)
    let ptyp_var ~loc s = core_type ~loc (Ptyp_var s)
    let ptyp_any ~loc = core_type ~loc Ptyp_any
    let ptyp typ = PTyp typ
    let pexp_extension_desc name payload = Pexp_extension (name, payload)
    let pexp_tuple ~loc l = expression ~loc (Pexp_tuple l)

    let pexp_variant ~loc v exp_opt =
      expression ~loc (Pexp_variant (v, exp_opt))

    let pstr_eval ~loc expr =
      PStr [ { pstr_desc = Pstr_eval (expr, []); pstr_loc = loc } ]

    let ppat_extension_desc name payload = Ppat_extension (name, payload)
    let ppat_tuple ~loc l = pattern ~loc (Ppat_tuple l)
    let ppat_var ~loc txt = pattern ~loc (Ppat_var { txt; loc })
    let ppat_any ~loc = pattern ~loc Ppat_any
    let ppat pat = PPat (pat, None)
    let closed_flag_to_string = function Closed -> "closed_" | Open -> "open_"
  end

  module Destruct = struct
    let ptyp = function PTyp typ -> Some typ | _ -> None

    let ptyp_tuple = function
      | { ptyp_desc = Ptyp_tuple typs; _ } -> Some typs
      | _ -> None

    let ptyp_var = function
      | { ptyp_desc = Ptyp_var s; _ } -> Some s
      | _ -> None

    let ptyp_any = function { ptyp_desc = Ptyp_any; _ } -> Some () | _ -> None

    let pstr_eval = function
      | PStr [ { pstr_desc = Pstr_eval (expr, []); _ } ] -> Some expr
      | _ -> None

    let pexp_tuple = function
      | { pexp_desc = Pexp_tuple l; _ } -> Some l
      | _ -> None

    let pexp_variant = function
      | { pexp_desc = Pexp_variant (s, e); _ } -> Some (s, e)
      | _ -> None

    let ppat = function PPat (pat, None) -> Some pat | _ -> None

    let ppat_tuple = function
      | { ppat_desc = Ppat_tuple pats; _ } -> Some pats
      | _ -> None

    let ppat_var = function
      | { ppat_desc = Ppat_var { txt; _ }; _ } -> Some txt
      | _ -> None

    let ppat_any = function { ppat_desc = Ppat_any; _ } -> Some () | _ -> None

    let closed_flag_from_string = function
      | "closed_" -> Some Closed
      | "open_" -> Some Open
      | _ -> None
  end
end

module Ast_502_arg = struct
  include Ast_502.Asttypes
  include Ast_502.Parsetree

  module Construct = struct
    let core_type ~loc ptyp_desc =
      { ptyp_desc; ptyp_loc = loc; ptyp_attributes = []; ptyp_loc_stack = [] }

    let expression ~loc pexp_desc =
      { pexp_desc; pexp_loc = loc; pexp_attributes = []; pexp_loc_stack = [] }

    let pattern ~loc ppat_desc =
      { ppat_desc; ppat_loc = loc; ppat_attributes = []; ppat_loc_stack = [] }

    let ptyp_extension_desc name payload = Ptyp_extension (name, payload)
    let ptyp_tuple ~loc typs = core_type ~loc (Ptyp_tuple typs)
    let ptyp_var ~loc s = core_type ~loc (Ptyp_var s)
    let ptyp_any ~loc = core_type ~loc Ptyp_any
    let ptyp typ = PTyp typ
    let pexp_extension_desc name payload = Pexp_extension (name, payload)
    let pexp_tuple ~loc l = expression ~loc (Pexp_tuple l)

    let pexp_variant ~loc v exp_opt =
      expression ~loc (Pexp_variant (v, exp_opt))

    let pstr_eval ~loc expr =
      PStr [ { pstr_desc = Pstr_eval (expr, []); pstr_loc = loc } ]

    let ppat_extension_desc name payload = Ppat_extension (name, payload)
    let ppat_tuple ~loc l = pattern ~loc (Ppat_tuple l)
    let ppat_var ~loc txt = pattern ~loc (Ppat_var { txt; loc })
    let ppat_any ~loc = pattern ~loc Ppat_any
    let ppat pat = PPat (pat, None)
    let closed_flag_to_string = function Closed -> "closed_" | Open -> "open_"
  end

  module Destruct = struct
    let ptyp = function PTyp typ -> Some typ | _ -> None

    let ptyp_tuple = function
      | { ptyp_desc = Ptyp_tuple typs; _ } -> Some typs
      | _ -> None

    let ptyp_var = function
      | { ptyp_desc = Ptyp_var s; _ } -> Some s
      | _ -> None

    let ptyp_any = function { ptyp_desc = Ptyp_any; _ } -> Some () | _ -> None

    let pstr_eval = function
      | PStr [ { pstr_desc = Pstr_eval (expr, []); _ } ] -> Some expr
      | _ -> None

    let pexp_tuple = function
      | { pexp_desc = Pexp_tuple l; _ } -> Some l
      | _ -> None

    let pexp_variant = function
      | { pexp_desc = Pexp_variant (s, e); _ } -> Some (s, e)
      | _ -> None

    let ppat = function PPat (pat, None) -> Some pat | _ -> None

    let ppat_tuple = function
      | { ppat_desc = Ppat_tuple pats; _ } -> Some pats
      | _ -> None

    let ppat_var = function
      | { ppat_desc = Ppat_var { txt; _ }; _ } -> Some txt
      | _ -> None

    let ppat_any = function { ppat_desc = Ppat_any; _ } -> Some () | _ -> None

    let closed_flag_from_string = function
      | "closed_" -> Some Closed
      | "open_" -> Some Open
      | _ -> None
  end
end

module To_503 = struct
  include Make (Ast_503_arg)
  open Ast_503.Asttypes
  open Ast_503.Parsetree

  let encode_bivariant_param typ inj =
    let loc = { typ.ptyp_loc with Location.loc_ghost = true } in
    let attr =
      {
        attr_name = { txt = Ext_name.bivariant_param; loc };
        attr_payload = PStr [];
        attr_loc = loc;
      }
    in
    ( { typ with ptyp_attributes = attr :: typ.ptyp_attributes },
      (NoVariance, inj) )

  let decode_bivariant_param (typ, (var, inj)) =
    let ptyp_attributes =
      List.without_first typ.ptyp_attributes ~pred:(fun attr ->
          String.equal attr.attr_name.txt Ext_name.bivariant_param)
    in
    match (ptyp_attributes, var) with
    | Some (_, ptyp_attributes), NoVariance ->
        Some ({ typ with ptyp_attributes }, inj)
    | None, _ -> None
    | Some _, _ -> invalid_encoding ~loc:typ.ptyp_loc "bivariant type parameter"

  let encode_bivariant_pstr ~loc pstr_desc =
    let loc = { loc with Location.loc_ghost = true } in
    let ext =
      ( { txt = Ext_name.bivariant_pstr; loc },
        PStr [ { pstr_loc = loc; pstr_desc } ] )
    in
    Pstr_extension (ext, [])

  let encode_bivariant_pstr_type ~loc rec_flag tds =
    encode_bivariant_pstr ~loc (Pstr_type (rec_flag, tds))

  let encode_bivariant_pstr_typext ~loc te =
    encode_bivariant_pstr ~loc (Pstr_typext te)

  let encode_bivariant_pstr_class ~loc cds =
    encode_bivariant_pstr ~loc (Pstr_class cds)

  let encode_bivariant_pstr_class_type ~loc ctds =
    encode_bivariant_pstr ~loc (Pstr_class_type ctds)

  let encode_bivariant_psig ~loc psig_desc =
    let loc = { loc with Location.loc_ghost = true } in
    let ext =
      ( { txt = Ext_name.bivariant_psig; loc },
        PSig [ { psig_loc = loc; psig_desc } ] )
    in
    Psig_extension (ext, [])

  let encode_bivariant_psig_type ~loc rec_flag tds =
    encode_bivariant_psig ~loc (Psig_type (rec_flag, tds))

  let encode_bivariant_psig_typesubst ~loc tds =
    encode_bivariant_psig ~loc (Psig_typesubst tds)

  let encode_bivariant_psig_typext ~loc te =
    encode_bivariant_psig ~loc (Psig_typext te)

  let encode_bivariant_psig_class ~loc cds =
    encode_bivariant_psig ~loc (Psig_class cds)

  let encode_bivariant_psig_class_type ~loc ctds =
    encode_bivariant_psig ~loc (Psig_class_type ctds)

  let decode_bivariant_pstr ~loc payload attributes =
    match (payload, attributes) with
    | ( PStr
          [
            {
              pstr_desc =
                (Pstr_type _ | Pstr_typext _ | Pstr_class _ | Pstr_class_type _)
                as x;
              _;
            };
          ],
        [] ) ->
        x
    | _ -> invalid_encoding ~loc "bivariant structure_item"

  let decode_bivariant_psig ~loc payload attributes =
    match (payload, attributes) with
    | ( PSig
          [
            {
              psig_desc =
                ( Psig_type _ | Psig_typesubst _ | Psig_typext _ | Psig_class _
                | Psig_class_type _ ) as x;
              _;
            };
          ],
        [] ) ->
        x
    | _ -> invalid_encoding ~loc "bivariant signature_item"

  let encode_bivariant_pmty_with ~loc mty constraints =
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
      ( { txt = Ext_name.bivariant_pmty_with; loc },
        PSig [ { psig_loc = loc; psig_desc } ] )
    in
    Pmty_extension ext

  let decode_bivariant_pmty_with ~loc payload =
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
                    { pmty_attributes = []; pmty_desc = Pmty_with _ as x; _ };
                  _;
                };
            _;
          };
        ] ->
        x
    | _ -> invalid_encoding ~loc "bivariant pmty_with"
end

module To_502 = struct
  include Make (Ast_502_arg)
end
