(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Inclusion checks for the core language *)

open Asttypes
open Path
open Types
open Mode
open Typedtree

type position = Errortrace.position = First | Second

(* Inclusion between value descriptions *)

type primitive_mismatch =
  | Name
  | Arity
  | No_alloc of position
  | Builtin
  | Effects
  | Coeffects
  | Native_name
  | Result_repr
  | Argument_repr of int
  | Layout_poly_attr

type value_mismatch =
  | Primitive_mismatch of primitive_mismatch
  | Not_a_primitive
  | Type of Errortrace.moregen_error
  | Zero_alloc of Zero_alloc.error
  | Modality of Mode.Modality.Value.error
  | Mode of Mode.Value.error

exception Dont_match of value_mismatch

type close_over_coercion = Env.locks * Longident.t * Location.t

type mmodes =
  | All
  | Specific of Mode.Value.l * Mode.Value.r * close_over_coercion option

let child_close_over_coercion_opt id c =
  match c with
  | None -> None
  | Some (locks, lid, loc) -> Some (locks, Longident.Ldot (lid, id), loc)

let child_modes id = function
  | All -> All
  | Specific (m0, m1, c) ->
    let c = child_close_over_coercion_opt id c in
    Specific (m0, m1, c)

let child_modes_with_modalities id ~modalities:(moda0, moda1) = function
  | All ->
    begin match Mode.Modality.Value.sub moda0 moda1 with
      | Ok () -> Ok All
      | Error e -> Error e
    end
  | Specific (m0, m1, c) ->
    let c = child_close_over_coercion_opt id c in
    begin match Mode.Modality.Value.to_const_opt moda1 with
    | None ->
      (* [wrap_constraint_with_shape] invokes inclusion check with
          identical modes and inferred modalities, which we workaround *)
      assert (moda0 == moda1);
      Mode.Value.submode_exn m0 m1;
      (* For children, we only check modality inclusion *)
      Ok All
    | Some moda1 ->
      let m0 = Mode.Modality.Value.apply moda0 m0 in
      let m1 = Mode.Modality.Value.Const.apply moda1 m1 in
      Ok (Specific (m0, m1, c))
    end

let check_modes env ?(crossing = Crossing.top) ~item ?typ = function
  | All -> Ok ()
  | Specific (m0, m1, c) ->
      let m0 =
        match c with
        | None -> m0
        | Some (locks, lid, loc) ->
            let m0 = Crossing.apply_left crossing m0 in
            let m0 = Env.walk_locks ~env ~loc lid ~item typ (m0, locks) in
            m0.mode
      in
      let m1 = Crossing.apply_right crossing m1 in
      Mode.Value.submode m0 m1

let native_repr_args nra1 nra2 =
  let rec loop i nra1 nra2 =
    match nra1, nra2 with
    | [], [] -> None
    | [], _ :: _ -> assert false
    | _ :: _, [] -> assert false
    | (_, nr1) :: nra1, (_, nr2) :: nra2 ->
      if not (Primitive.equal_native_repr nr1 nr2) then Some (Argument_repr i)
      else loop (i+1) nra1 nra2
  in
  loop 1 nra1 nra2

let primitive_descriptions pd1 pd2 =
  let open Primitive in
  if not (String.equal pd1.prim_name pd2.prim_name) then
    Some Name
  else if not (Int.equal pd1.prim_arity pd2.prim_arity) then
    Some Arity
  else if (not pd1.prim_alloc) && pd2.prim_alloc then
    Some (No_alloc First)
  else if pd1.prim_alloc && (not pd2.prim_alloc) then
    Some (No_alloc Second)
  else if not
    (Bool.equal pd1.prim_is_layout_poly
                pd2.prim_is_layout_poly) then
    Some Layout_poly_attr
  else if not (Bool.equal pd1.prim_c_builtin pd2.prim_c_builtin) then
    Some Builtin
  else if not (Primitive.equal_effects pd1.prim_effects pd2.prim_effects) then
    Some Effects
  else if not
    (Primitive.equal_coeffects
       pd1.prim_coeffects pd2.prim_coeffects) then
    Some Coeffects
  else if not (String.equal pd1.prim_native_name pd2.prim_native_name) then
    Some Native_name
  else if not
    (match pd1.prim_native_repr_res, pd2.prim_native_repr_res with
      | (_, nr1), (_, nr2) -> Primitive.equal_native_repr nr1 nr2) then
    Some Result_repr
  else
    native_repr_args pd1.prim_native_repr_args pd2.prim_native_repr_args

let value_descriptions ~loc env name
    ~mmodes
    (vd1 : Types.value_description)
    (vd2 : Types.value_description) =
  Builtin_attributes.check_alerts_inclusion
    ~def:vd1.val_loc
    ~use:vd2.val_loc
    loc
    vd1.val_attributes vd2.val_attributes
    name;
  begin match Zero_alloc.sub vd1.val_zero_alloc vd2.val_zero_alloc with
  | Ok () -> ()
  | Error e -> raise (Dont_match (Zero_alloc e))
  end;
  let crossing = Ctype.crossing_of_ty env vd2.val_type in
  let modalities = vd1.val_modalities, vd2.val_modalities in
  let modes =
    match child_modes_with_modalities name ~modalities mmodes with
    | Ok modes -> modes
    | Error e -> raise (Dont_match (Modality e))
  in
  begin match check_modes env ~crossing ~item:Value ~typ:vd1.val_type modes with
  | Ok () -> ()
  | Error e -> raise (Dont_match (Mode e))
  end;
  match vd1.val_kind with
  | Val_prim p1 -> begin
     match vd2.val_kind with
     | Val_prim p2 -> begin
         let locality = [ Mode.Locality.global; Mode.Locality.local ] in
         let yielding = [ Mode.Yielding.unyielding; Mode.Yielding.yielding ] in
         List.iter (fun loc ->
           List.iter (fun yield ->
             let ty1, _, _, _ = Ctype.instance_prim p1 vd1.val_type in
             let ty2, mode_l2, mode_y2, _ = Ctype.instance_prim p2 vd2.val_type in
             Option.iter (Mode.Locality.equate_exn loc) mode_l2;
             Option.iter (Mode.Yielding.equate_exn yield) mode_y2;
             try
               Ctype.moregeneral env true ty1 ty2
             with Ctype.Moregen err ->
               raise (Dont_match (Type err))
           ) yielding
         ) locality;
         match primitive_descriptions p1 p2 with
         | None -> Tcoerce_none
         | Some err -> raise (Dont_match (Primitive_mismatch err))
       end
     | _ ->
        let ty1, mode_l1, _, sort1 = Ctype.instance_prim p1 vd1.val_type in
        (try Ctype.moregeneral env true ty1 vd2.val_type
         with Ctype.Moregen err -> raise (Dont_match (Type err)));
        let pc =
          {pc_desc = p1; pc_type = vd2.Types.val_type;
           pc_poly_mode = Option.map Mode.Locality.disallow_right mode_l1;
           pc_poly_sort=sort1;
           pc_env = env; pc_loc = vd1.Types.val_loc; } in
        Tcoerce_primitive pc
     end
  | _ ->
     match Ctype.moregeneral env true vd1.val_type vd2.val_type with
     | exception Ctype.Moregen err -> raise (Dont_match (Type err))
     | () -> begin
       match vd2.val_kind with
         | Val_prim _ -> raise (Dont_match Not_a_primitive)
         | _ -> Tcoerce_none
     end

(* Inclusion between manifest types (particularly for private row types) *)

let is_absrow env ty =
  match get_desc ty with
  | Tconstr(Pident _, _, _) ->
      (* This function is checking for an abstract row on the side that is being
         included into (usually numbered with "2" in this file).  In this case,
         the abstract row variable has been substituted for an object or variant
         type. *)
      begin match get_desc (Ctype.expand_head env ty) with
      | Tobject _|Tvariant _ -> true
      | _ -> false
      end
  | _ -> false

(* Inclusion between type declarations *)

let choose ord first second =
  match ord with
  | First -> first
  | Second -> second

let choose_other ord first second =
  match ord with
  | First -> choose Second first second
  | Second -> choose First first second

(* Documents which kind of private thing would be revealed *)
type privacy_mismatch =
  | Private_type_abbreviation
  | Private_variant_type
  | Private_record_type
  | Private_record_unboxed_product_type
  | Private_extensible_variant
  | Private_row_type

type type_kind =
  | Kind_abstract
  | Kind_record
  | Kind_record_unboxed_product
  | Kind_variant
  | Kind_open

let of_kind = function
  | Type_abstract _ -> Kind_abstract
  | Type_record (_, _, _) -> Kind_record
  | Type_record_unboxed_product (_, _, _) -> Kind_record_unboxed_product
  | Type_variant (_, _, _) -> Kind_variant
  | Type_open -> Kind_open

type kind_mismatch = type_kind * type_kind

type label_mismatch =
  | Type of Errortrace.equality_error
  | Mutability of position
  | Atomicity of position
  | Modality of Modality.Value.equate_error

type record_change =
  (Types.label_declaration, Types.label_declaration, label_mismatch)
    Diffing_with_keys.change

type record_mismatch =
  | Label_mismatch of record_change list
  | Inlined_representation of position
  | Float_representation of position
  | Ufloat_representation of position
  | Mixed_representation of position
  | Mixed_representation_with_flat_floats of position

type constructor_mismatch =
  | Type of Errortrace.equality_error
  | Arity
  | Inline_record of record_change list
  | Kind of position
  | Explicit_return_type of position
  | Modality of int * Modality.Value.equate_error

type extension_constructor_mismatch =
  | Constructor_privacy
  | Constructor_mismatch of Ident.t
                            * Types.extension_constructor
                            * Types.extension_constructor
                            * constructor_mismatch

type private_variant_mismatch =
  | Only_outer_closed (* It's only dangerous in one direction *)
  | Missing of position * string
  | Presence of string
  | Incompatible_types_for of string
  | Types of Errortrace.equality_error

type private_object_mismatch =
  | Missing of string
  | Types of Errortrace.equality_error

type variant_change =
  (Types.constructor_declaration as 'l, 'l, constructor_mismatch)
    Diffing_with_keys.change

type unsafe_mode_crossing_mismatch =
  | Mode_crossing_only_on of position
  | Bounds_not_equal of unsafe_mode_crossing * unsafe_mode_crossing

type type_mismatch =
  | Arity
  | Privacy of privacy_mismatch
  | Kind of kind_mismatch
  | Constraint of Errortrace.equality_error
  | Manifest of Errortrace.equality_error
  | Parameter_jkind of type_expr * Jkind.Violation.t
  | Private_variant of type_expr * type_expr * private_variant_mismatch
  | Private_object of type_expr * type_expr * private_object_mismatch
  | Variance
  | Record_mismatch of record_mismatch
  | Variant_mismatch of variant_change list
  | Unboxed_representation of position * attributes
  | Extensible_representation of position
  | With_null_representation of position
  | Jkind of Jkind.Violation.t
  | Unsafe_mode_crossing of unsafe_mode_crossing_mismatch

let report_modality_sub_error first second ppf e =
  let print_modality id ppf m =
    Printtyp.modality ~id:(fun ppf -> Format.pp_print_string ppf id) ppf m
  in
  let Modality.Value.Error(ax, {left; right}) = e in
  Format.fprintf ppf "%s is %a and %s is %a."
    (String.capitalize_ascii second)
    (print_modality "empty") (Atom (ax, right) : Modality.t)
    first
    (print_modality "not") (Atom (ax, left) : Modality.t)

let report_mode_sub_error got expected ppf e =
  let Mode.Value.Error(ax, {left; right}) = e in
  match ax with
  | Comonadic Areality -> Format.fprintf ppf "This escapes its region."
  | _ ->
    Format.fprintf ppf "%s %a but %s %a."
      (String.capitalize_ascii got)
      (Misc.Style.as_inline_code (Value.Const.print_axis ax)) left
      expected
      (Misc.Style.as_inline_code (Value.Const.print_axis ax)) right

let report_modality_equate_error first second ppf ((equate_step, sub_error) : Modality.Value.equate_error) =
  match equate_step with
  | Left_le_right -> report_modality_sub_error first second ppf sub_error
  | Right_le_left -> report_modality_sub_error second first ppf sub_error

module Style = Misc.Style

let report_primitive_mismatch first second ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  match (err : primitive_mismatch) with
  | Name ->
      pr "The names of the primitives are not the same"
  | Arity ->
      pr "The syntactic arities of these primitives were not the same.@ \
          (They must have the same number of arrows present in the source.)"
  | No_alloc ord ->
      pr "%s primitive is %a but %s is not"
        (String.capitalize_ascii (choose ord first second))
        Style.inline_code "[@@noalloc]"
        (choose_other ord first second)
  | Builtin ->
      pr "The two primitives differ in whether they are builtins"
  | Effects ->
      pr "The two primitives have different effect annotations"
  | Coeffects ->
      pr "The two primitives have different coeffect annotations"
  | Native_name ->
      pr "The native names of the primitives are not the same"
  | Result_repr ->
      pr "The two primitives' results have different representations"
  | Argument_repr n ->
      pr "The two primitives' %d%s arguments have different representations"
        n (Misc.ordinal_suffix n)
  | Layout_poly_attr ->
      pr "The two primitives have different [@@layout_poly] attributes"

let report_value_mismatch first second env ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  pr "@ ";
  match (err : value_mismatch) with
  | Primitive_mismatch pm ->
      report_primitive_mismatch first second ppf pm
  | Not_a_primitive ->
      pr "The implementation is not a primitive."
  | Type trace ->
      Printtyp.report_moregen_error ppf Type_scheme env trace
        (fun ppf -> Format.fprintf ppf "The type")
        (fun ppf -> Format.fprintf ppf "is not compatible with the type")
  | Zero_alloc e -> Zero_alloc.print_error ppf e
  | Modality e -> report_modality_sub_error first second ppf e
  | Mode e ->
      let got = first ^ " is" in
      let expected = second ^ " is" in
      report_mode_sub_error got expected ppf e

let report_type_inequality env ppf err =
  Printtyp.report_equality_error ppf Type_scheme env err
    (fun ppf -> Format.fprintf ppf "The type")
    (fun ppf -> Format.fprintf ppf "is not equal to the type")

let report_privacy_mismatch ppf err =
  let singular, item =
    match err with
    | Private_type_abbreviation  -> true,  "type abbreviation"
    | Private_variant_type       -> false, "variant constructor(s)"
    | Private_record_type        -> true,  "record constructor"
    | Private_record_unboxed_product_type -> true, "unboxed record constructor"
    | Private_extensible_variant -> true,  "extensible variant"
    | Private_row_type           -> true,  "row type"
  in Format.fprintf ppf "%s %s would be revealed."
       (if singular then "A private" else "Private")
       item

let report_label_mismatch first second env ppf err =
  match (err : label_mismatch) with
  | Type err ->
      report_type_inequality env ppf err
  | Mutability ord ->
      Format.fprintf ppf "%s is mutable and %s is not."
        (String.capitalize_ascii (choose ord first second))
        (choose_other ord first second)
  | Atomicity ord ->
      Format.fprintf ppf "%s is atomic and %s is not."
        (String.capitalize_ascii (choose ord first second))
        (choose_other ord first second)
  | Modality err_ -> report_modality_equate_error first second ppf err_

let pp_record_diff first second prefix decl env ppf (x : record_change) =
  match x with
  | Delete cd ->
      Format.fprintf ppf "%aAn extra field, %a, is provided in %s %s."
        prefix x Style.inline_code (Ident.name cd.delete.ld_id) first decl
  | Insert cd ->
      Format.fprintf  ppf "%aA field, %a, is missing in %s %s."
        prefix x Style.inline_code (Ident.name cd.insert.ld_id) first decl
  | Change Type {got=lbl1; expected=lbl2; reason} ->
      Format.fprintf ppf
        "@[<hv>%aFields do not match:@;<1 2>\
         %a@ is not the same as:\
         @;<1 2>%a@ %a@]"
        prefix x
        (Style.as_inline_code Printtyp.label) lbl1
        (Style.as_inline_code Printtyp.label) lbl2
        (report_label_mismatch first second env) reason
  | Change Name n ->
      Format.fprintf ppf "%aFields have different names, %a and %a."
        prefix x
        Style.inline_code n.got
        Style.inline_code n.expected
  | Swap sw ->
      Format.fprintf ppf "%aFields %a and %a have been swapped."
        prefix x
        Style.inline_code sw.first
        Style.inline_code sw.last
  | Move {name; got; expected } ->
      Format.fprintf ppf
        "@[<2>%aField %a has been moved@ from@ position %d@ to %d.@]"
        prefix x Style.inline_code name expected got

let report_patch pr_diff first second decl env ppf patch =
  let nl ppf () = Format.fprintf ppf "@," in
  let no_prefix _ppf _ = () in
  match patch with
  | [ elt ] ->
      Format.fprintf ppf "@[<hv>%a@]"
        (pr_diff first second no_prefix decl env) elt
  | _ ->
      let pp_diff = pr_diff first second Diffing_with_keys.prefix decl env in
      Format.fprintf ppf "@[<hv>%a@]"
        (Format.pp_print_list ~pp_sep:nl pp_diff) patch

let report_record_mismatch first second decl env ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  match err with
  | Label_mismatch patch ->
      report_patch pp_record_diff first second decl env ppf patch
  | Inlined_representation ord ->
      pr "@[<hv>Their internal representations differ:@ %s %s %s.@]"
        (choose ord first second) decl
        "is an inlined record"
  | Float_representation ord ->
      pr "@[<hv>Their internal representations differ:@ %s %s %s.@]"
        (choose ord first second) decl
        "uses unboxed float representation"
  | Ufloat_representation ord ->
      (* CR layouts: This case should unreachable now.  But it may be reachable
         when we allow [any] types in structure declarations, using an example
         like the "unboxed float representation" one in
         [typing-unboxed-types/test.ml].  Add a test then. *)
      pr "@[<hv>Their internal representations differ:@ %s %s %s.@]"
        (choose ord first second) decl
        "uses float# representation"
  | Mixed_representation ord ->
      (* CR layouts: As above. *)
      pr "@[<hv>Their internal representations differ:@ %s %s %s.@]"
        (choose ord first second) decl
        "uses mixed representation"
  | Mixed_representation_with_flat_floats ord ->
      pr "@[<hv>Their internal representations differ:@ %s %s %s.@]"
        (choose ord first second) decl
        "uses a mixed representation where boxed floats are stored flat"

let report_constructor_mismatch first second decl env ppf err =
  let pr fmt  = Format.fprintf ppf fmt in
  match (err : constructor_mismatch) with
  | Type err -> report_type_inequality env ppf err
  | Arity -> pr "They have different arities."
  | Inline_record err ->
      report_patch pp_record_diff first second decl env ppf err
  | Kind ord ->
      pr "%s uses inline records and %s doesn't."
        (String.capitalize_ascii (choose ord first second))
        (choose_other ord first second)
  | Explicit_return_type ord ->
      pr "%s has explicit return type and %s doesn't."
        (String.capitalize_ascii (choose ord first second))
        (choose_other ord first second)
  | Modality (i, err) ->
      pr "Modality mismatch at argument position %i:@ %a"
        (i + 1) (report_modality_equate_error first second) err
        (* argument position is one-based; more intuitive *)

let pp_variant_diff first second prefix decl env ppf (x : variant_change) =
  match x with
  | Delete cd ->
      Format.fprintf ppf  "%aAn extra constructor, %a, is provided in %s %s."
        prefix x Style.inline_code (Ident.name cd.delete.cd_id) first decl
  | Insert cd ->
      Format.fprintf ppf "%aA constructor, %a, is missing in %s %s."
        prefix x Style.inline_code (Ident.name cd.insert.cd_id) first decl
  | Change Type {got; expected; reason} ->
      Format.fprintf ppf
        "@[<hv>%aConstructors do not match:@;<1 2>\
         %a@ is not the same as:\
         @;<1 2>%a@ %a@]"
        prefix x
        (Style.as_inline_code Printtyp.constructor) got
        (Style.as_inline_code Printtyp.constructor) expected
        (report_constructor_mismatch first second decl env) reason
  | Change Name n ->
      Format.fprintf ppf
        "%aConstructors have different names, %a and %a."
        prefix x
        Style.inline_code n.got
        Style.inline_code n.expected
  | Swap sw ->
      Format.fprintf ppf
        "%aConstructors %a and %a have been swapped."
        prefix x
        Style.inline_code sw.first
        Style.inline_code sw.last
  | Move {name; got; expected} ->
      Format.fprintf ppf
        "@[<2>%aConstructor %a has been moved@ from@ position %d@ to %d.@]"
        prefix x Style.inline_code name expected got

let report_extension_constructor_mismatch first second decl env ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  match (err : extension_constructor_mismatch) with
  | Constructor_privacy ->
      pr "Private extension constructor(s) would be revealed."
  | Constructor_mismatch (id, ext1, ext2, err) ->
      let constructor =
        Style.as_inline_code (Printtyp.extension_only_constructor id)
      in
      pr "@[<hv>Constructors do not match:@;<1 2>%a@ is not the same as:\
          @;<1 2>%a@ %a@]"
        constructor ext1
        constructor ext2
        (report_constructor_mismatch first second decl env) err


let report_private_variant_mismatch first second decl env ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  let pp_tag ppf x = Format.fprintf ppf "`%s" x in
  match (err : private_variant_mismatch) with
  | Only_outer_closed ->
      (* It's only dangerous in one direction, so we don't have a position *)
      pr "%s is private and closed, but %s is not closed"
        (String.capitalize_ascii second) first
  | Missing (ord, name) ->
      pr "The constructor %a is only present in %s %s."
        Style.inline_code name (choose ord first second) decl
  | Presence s ->
      pr "The tag %a is present in the %s %s,@ but might not be in the %s"
        (Style.as_inline_code pp_tag) s second decl first
  | Incompatible_types_for s -> pr "Types for tag `%s are incompatible" s
  | Types err ->
      report_type_inequality env ppf err

let report_private_object_mismatch env ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  match (err : private_object_mismatch) with
  | Missing s ->
      pr "The implementation is missing the method %a" Style.inline_code s
  | Types err -> report_type_inequality env ppf err

let report_kind_mismatch first second ppf (kind1, kind2) =
  let pr fmt = Format.fprintf ppf fmt in
  let kind_to_string = function
  | Kind_abstract -> "abstract"
  | Kind_record -> "a record"
  | Kind_record_unboxed_product -> "an unboxed record"
  | Kind_variant -> "a variant"
  | Kind_open -> "an extensible variant" in
  pr "%s is %s, but %s is %s."
    (String.capitalize_ascii first)
    (kind_to_string kind1)
    second
    (kind_to_string kind2)

let print_unsafe_mode_crossing ppf umc =
  Format.fprintf ppf "mod %a@ %a"
    Mode.Crossing.print umc.unsafe_mod_bounds
    Jkind.With_bounds.format umc.unsafe_with_bounds

let report_unsafe_mode_crossing_mismatch first second ppf e =
  let pr fmt = Format.fprintf ppf fmt in
  match e with
  | Mode_crossing_only_on ord ->
    pr "%s has [%@%@unsafe_allow_any_mode_crossing], but %s does not"
      (choose ord first second)
      (choose_other ord first second)
  | Bounds_not_equal (first_umc, second_umc) ->
    (* CR layouts v2.8: It'd be nice to specifically highlight the offending axis,
       rather than printing all axes here. *)
    pr "Both specify [%@%@unsafe_allow_any_mode_crossing], but their \
        bounds are not equal@,\
        @[%s has:@ %a@]@ \
        @[but %s has:@ %a@]"
      first print_unsafe_mode_crossing first_umc
      second print_unsafe_mode_crossing second_umc

let report_type_mismatch first second decl env ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  pr "@ ";
  match err with
  | Arity ->
      pr "They have different arities."
  | Privacy err ->
      report_privacy_mismatch ppf err
  | Kind err ->
      report_kind_mismatch first second ppf err
  | Constraint err ->
      (* This error can come from implicit parameter disagreement or from
         explicit `constraint`s.  Both affect the parameters, hence this choice
         of explanatory text *)
      pr "Their parameters differ:@,";
      report_type_inequality env ppf err
  | Manifest err ->
      report_type_inequality env ppf err
  | Parameter_jkind (ty, v) ->
      pr "The problem is in the kinds of a parameter:@,";
      Jkind.Violation.report_with_offender
        ~offender:(fun pp -> Printtyp.type_expr pp ty) ppf v
  | Private_variant (_ty1, _ty2, mismatch) ->
      report_private_variant_mismatch first second decl env ppf mismatch
  | Private_object (_ty1, _ty2, mismatch) ->
      report_private_object_mismatch env ppf mismatch
  | Variance ->
      pr "Their variances do not agree."
  | Record_mismatch err ->
      report_record_mismatch first second decl env ppf err
  | Variant_mismatch err ->
      report_patch pp_variant_diff first second decl env ppf err
  | Unboxed_representation (ord, attrs) ->
      pr "Their internal representations differ:@ %s %s %s."
         (choose ord first second) decl
         "uses unboxed representation";
      if Builtin_attributes.has_unboxed attrs then
        pr "@ Hint: %s %s has [%@unboxed]. Did you mean [%@%@unboxed]?"
          (choose ord second first) decl
  | Extensible_representation ord ->
      pr "Their internal representations differ:@ %s %s %s."
         (choose ord first second) decl
         "is extensible"
  | With_null_representation ord ->
      pr "Their internal representations differ:@ %s %s %s."
         (choose ord first second) decl
         "has a constructor represented as a null pointer";
      pr "@ Hint: add [%@%@or_null_reexport]."
  | Jkind v ->
      Jkind.Violation.report_with_name ~name:first ppf v
  | Unsafe_mode_crossing mismatch ->
    pr "They have different unsafe mode crossing behavior:@,@[<v 2>%a@]"
      (fun ppf (first, second, mismatch) ->
         report_unsafe_mode_crossing_mismatch first second ppf mismatch)
      (first, second, mismatch)

let compare_unsafe_mode_crossing ~env umc1 umc2 =
  match umc1, umc2 with
  | None, None -> None
  | Some _, None -> Some (Unsafe_mode_crossing (Mode_crossing_only_on First))
  | None, Some _ -> Some (Unsafe_mode_crossing (Mode_crossing_only_on Second))
  | Some umc1, Some umc2 ->
    if equal_unsafe_mode_crossing
         ~type_equal:(Ctype.type_equal env)
         umc1 umc2
    then None
    else
      Some (
        Unsafe_mode_crossing (
          Bounds_not_equal (umc1, umc2)))

module Record_diffing = struct

  let compare_labels env params1 params2
        (ld1 : Types.label_declaration)
        (ld2 : Types.label_declaration) =
        let err =
          match ld1.ld_mutable, ld2.ld_mutable with
          | Immutable, Immutable -> None
          | Mutable _, Immutable -> Some (Mutability First)
          | Immutable, Mutable _ -> Some (Mutability Second)
          | Mutable { mode = m1; atomic = atomic1 },
            Mutable { mode = m2; atomic = atomic2 } ->
            begin match atomic1, atomic2 with
            | Atomic, Nonatomic -> Some (Atomicity First)
            | Nonatomic, Atomic -> Some (Atomicity Second)
            | Atomic, Atomic | Nonatomic, Nonatomic ->
                let open Mode.Value.Comonadic in
                equate_exn m1 legacy;
                equate_exn m2 legacy;
                None
            end
        in
        begin match err with
        | Some err -> Some err
        | None ->
          match
            Modality.Value.Const.equate ld1.ld_modalities ld2.ld_modalities
          with
          | Ok () ->
            let tl1 = params1 @ [ld1.ld_type] in
            let tl2 = params2 @ [ld2.ld_type] in
            begin
            (* Allow renaming: this gets called for inline records in GADT
               constructors that may have existentials. *)
            match Ctype.equal env true tl1 tl2 with
            | exception Ctype.Equality err ->
                Some (Type err : label_mismatch)
            | () -> None
            end
          | Error e -> Some (Modality e : label_mismatch)
        end

  let rec equal ~loc env params1 params2
      (labels1 : Types.label_declaration list)
      (labels2 : Types.label_declaration list) =
    match labels1, labels2 with
    | [], [] -> true
    | _ :: _ , [] | [], _ :: _ -> false
    | ld1 :: rem1, ld2 :: rem2 ->
        if Ident.name ld1.ld_id <> Ident.name ld2.ld_id
        then false
        else begin
          Builtin_attributes.check_deprecated_mutable_inclusion
            ~def:ld1.ld_loc
            ~use:ld2.ld_loc
            loc
            ld1.ld_attributes ld2.ld_attributes
            (Ident.name ld1.ld_id);
          match compare_labels env params1 params2 ld1 ld2 with
          | Some _ -> false
          (* add arguments to the parameters, cf. PR#7378 *)
          | None ->
              equal ~loc env
                (ld1.ld_type::params1) (ld2.ld_type::params2)
                rem1 rem2
        end

  module Defs = struct
    type left = Types.label_declaration
    type right = left
    type diff = label_mismatch
    type state = type_expr list * type_expr list
  end
  module Diff = Diffing_with_keys.Define(Defs)

  let update (d:Diff.change) (params1,params2 as st) =
    match d with
    | Insert _ | Change _ | Delete _ -> st
    | Keep (x,y,_) ->
        (* We need to add equality between existential type parameters
           (in inline records) *)
        x.data.ld_type::params1, y.data.ld_type::params2

  let test _loc env (params1,params2)
      ({pos; data=lbl1}: Diff.left)
      ({data=lbl2; _ }: Diff.right)
    =
    let name1, name2 = Ident.name lbl1.ld_id, Ident.name lbl2.ld_id in
    if  name1 <> name2 then
      let types_match =
        match compare_labels env params1 params2 lbl1 lbl2 with
        | Some _ -> false
        | None -> true
      in
      Error
        (Diffing_with_keys.Name {types_match; pos; got=name1; expected=name2})
    else
      match compare_labels env params1 params2 lbl1 lbl2 with
      | Some reason ->
          Error (
            Diffing_with_keys.Type {pos; got=lbl1; expected=lbl2; reason}
          )
      | None -> Ok ()

  let weight: Diff.change -> _ = function
    | Insert _ -> 10
    | Delete _ -> 10
    | Keep _ -> 0
    | Change (_,_,Diffing_with_keys.Name t ) ->
        if t.types_match then 10 else 15
    | Change _ -> 10



  let key (x: Defs.left) = Ident.name x.ld_id
  let diffing loc env params1 params2 cstrs_1 cstrs_2 =
    let module Compute = Diff.Simple(struct
        let key_left = key
        let key_right = key
        let update = update
        let test = test loc env
        let weight = weight
      end)
    in
    Compute.diff (params1,params2) cstrs_1 cstrs_2

  let compare ~loc env params1 params2 l r =
    if equal ~loc env params1 params2 l r then
      None
    else
      Some (diffing loc env params1 params2 l r)

  let find_mismatch_in_mixed_record_representations
      (s1 : mixed_product_shape) (s2 : mixed_product_shape)
    =
    if s1 = s2 then None
    else
      let has_float_boxed_on_read fields =
        Array.exists (function
            | Float_boxed -> true
            | _ -> false)
          fields
      in
      if has_float_boxed_on_read s1
      then Some (Mixed_representation_with_flat_floats First)
      else if has_float_boxed_on_read s2
      then Some (Mixed_representation_with_flat_floats Second)
      else
        Misc.fatal_error
          "Impossible: the only way for mixed blocks to differ in \
           representation is if one is a flat float record with a boxed float \
           field, and the other isn't."

  let compare_with_representation (type rep) ~loc
        (record_form : rep record_form) env params1 params2 l r
        (rep1 : rep) (rep2 : rep) =
    if not (equal ~loc env params1 params2 l r) then
      let patch = diffing loc env params1 params2 l r in
      Some (Record_mismatch (Label_mismatch patch))
    else
      match record_form with
      | Legacy ->
        begin match rep1, rep2 with
        | Record_unboxed, Record_unboxed -> None
        | Record_unboxed, _ -> Some (Unboxed_representation (First, []))
        | _, Record_unboxed -> Some (Unboxed_representation (Second, []))

        | Record_inlined _, Record_inlined _ -> None
        | Record_inlined _, _ ->
           Some (Record_mismatch (Inlined_representation First))
        | _, Record_inlined _ ->
           Some (Record_mismatch (Inlined_representation Second))

        | Record_float, Record_float -> None
        | Record_float, _ ->
           Some (Record_mismatch (Float_representation First))
        | _, Record_float ->
           Some (Record_mismatch (Float_representation Second))

        | Record_ufloat, Record_ufloat -> None
        | Record_ufloat, _ ->
           Some (Record_mismatch (Ufloat_representation First))
        | _, Record_ufloat ->
           Some (Record_mismatch (Ufloat_representation Second))

        | Record_mixed m1, Record_mixed m2 ->
            begin match find_mismatch_in_mixed_record_representations m1 m2 with
            | None -> None
            | Some mismatch -> Some (Record_mismatch mismatch)
            end
        | Record_mixed _, _ ->
           Some (Record_mismatch (Mixed_representation First))
        | _, Record_mixed _ ->
           Some (Record_mismatch (Mixed_representation Second))

        | Record_boxed _, Record_boxed _ -> None
        end
      | Unboxed_product ->
        begin match rep1, rep2 with
        | Record_unboxed_product, Record_unboxed_product -> None
        end
end

(* just like List.find_map, but also gives index if found *)
let rec find_map_idx f ?(off = 0) l =
  match l with
  | [] -> None
  | x :: xs -> begin
      match f x with
      | None -> find_map_idx f ~off:(off+1) xs
      | Some y -> Some (off, y)
    end

let get_error = function
  | Ok () -> None
  | Error e -> Some e

module Variant_diffing = struct

  let compare_constructor_arguments ~loc env params1 params2 arg1 arg2 =
    match arg1, arg2 with
    | Types.Cstr_tuple arg1, Types.Cstr_tuple arg2 ->
        if List.length arg1 <> List.length arg2 then
          Some (Arity : constructor_mismatch)
        else begin
          let type_and_mode (ca : Types.constructor_argument) = ca.ca_type, ca.ca_modalities in
          let arg1_tys, arg1_gfs = List.split (List.map type_and_mode arg1)
          and arg2_tys, arg2_gfs = List.split (List.map type_and_mode arg2)
          in
          (* Ctype.equal must be called on all arguments at once, cf. PR#7378 *)
          (* Allow renaming: in the GADT case, these arguments are distinct from
             type parameters (which have been unified). See also
             Note [Contravariance of type parameter jkinds]. *)
          match Ctype.equal env true (params1 @ arg1_tys) (params2 @ arg2_tys) with
          | exception Ctype.Equality err -> Some (Type err)
          | () -> List.combine arg1_gfs arg2_gfs
                  |> find_map_idx
                    (fun (x,y) -> get_error @@ Modality.Value.Const.equate x y)
                  |> Option.map (fun (i, err) -> Modality (i, err))
        end
    | Types.Cstr_record l1, Types.Cstr_record l2 ->
        Option.map
          (fun rec_err -> Inline_record rec_err)
          (Record_diffing.compare env ~loc params1 params2 l1 l2)
    | Types.Cstr_record _, _ -> Some (Kind First : constructor_mismatch)
    | _, Types.Cstr_record _ -> Some (Kind Second : constructor_mismatch)

  let compare_constructors ~loc env params1 params2 res1 res2 args1 args2 =
    match res1, res2 with
    | Some r1, Some r2 ->
        (* Allow renaming here: variables in GADT-syntax constructors are
           distinct from the variables in type parameters *)
        begin match Ctype.equal env true [r1] [r2] with
        | exception Ctype.Equality err -> Some (Type err)
              (* Pass the result types in this call to
                 [compare_constructor_arguments], so that the call to [Ctype.equal]
                 can see the entire scope of the variables *)
        | () -> compare_constructor_arguments ~loc env [r1] [r2] args1 args2
        end
    | Some _, None -> Some (Explicit_return_type First)
    | None, Some _ -> Some (Explicit_return_type Second)
    | None, None ->
        compare_constructor_arguments ~loc env params1 params2 args1 args2

  let equal ~loc env params1 params2
      (cstrs1 : Types.constructor_declaration list)
      (cstrs2 : Types.constructor_declaration list) =
    List.length cstrs1 = List.length cstrs2 &&
    List.for_all2 (fun (cd1:Types.constructor_declaration)
                    (cd2:Types.constructor_declaration) ->
        Ident.name cd1.cd_id = Ident.name cd2.cd_id
        &&
        begin
          Builtin_attributes.check_alerts_inclusion
            ~def:cd1.cd_loc
            ~use:cd2.cd_loc
            loc
            cd1.cd_attributes cd2.cd_attributes
            (Ident.name cd1.cd_id)
          ;
        match compare_constructors ~loc env params1 params2
                cd1.cd_res cd2.cd_res cd1.cd_args cd2.cd_args with
        | Some _ -> false
        | None -> true
      end) cstrs1 cstrs2

  module Defs = struct
    type left = Types.constructor_declaration
    type right = left
    type diff = constructor_mismatch
    type state = type_expr list * type_expr list
  end
  module D = Diffing_with_keys.Define(Defs)

  let update _ st = st

  let weight: D.change -> _ = function
    | Insert _ -> 10
    | Delete _ -> 10
    | Keep _ -> 0
    | Change (_,_,Diffing_with_keys.Name t) ->
        if t.types_match then 10 else 15
    | Change _ -> 10


  let test loc env (params1,params2)
      ({pos; data=cd1}: D.left)
      ({data=cd2; _}: D.right) =
    let name1, name2 = Ident.name cd1.cd_id, Ident.name cd2.cd_id in
    if  name1 <> name2 then
      let types_match =
        match compare_constructors ~loc env params1 params2
                cd1.cd_res cd2.cd_res cd1.cd_args cd2.cd_args with
        | Some _ -> false
        | None -> true
      in
      Error
        (Diffing_with_keys.Name {types_match; pos; got=name1; expected=name2})
    else
      match compare_constructors ~loc env params1 params2
              cd1.cd_res cd2.cd_res cd1.cd_args cd2.cd_args with
      | Some reason ->
          Error (Diffing_with_keys.Type {pos; got=cd1; expected=cd2; reason})
      | None -> Ok ()

  let diffing loc env params1 params2 cstrs_1 cstrs_2 =
    let key (x:Defs.left) = Ident.name x.cd_id in
    let module Compute = D.Simple(struct
        let key_left = key
        let key_right = key
        let test = test loc env
        let update = update
        let weight = weight
      end)
    in
    Compute.diff (params1,params2) cstrs_1 cstrs_2

  let compare ~loc env params1 params2 l r =
    if equal ~loc env params1 params2 l r then
      None
    else
      Some (diffing loc env params1 params2 l r)

  let compare_with_representation ~loc env params1 params2
      cstrs1 cstrs2 rep1 rep2
    =
    let err = compare ~loc env params1 params2 cstrs1 cstrs2 in
    let attrs_of_only cstrs =
      match cstrs with
      | [cstr] -> cstr.Types.cd_attributes
      | _ -> []
    in
    match err, rep1, rep2 with
    | None, Variant_unboxed, Variant_unboxed
    | None, Variant_boxed _, Variant_boxed _
    | None, Variant_extensible, Variant_extensible
    | None, Variant_with_null, Variant_with_null -> None
    | Some err, _, _ ->
        Some (Variant_mismatch err)
    | None, Variant_unboxed, Variant_boxed _ ->
        Some (Unboxed_representation (First, attrs_of_only cstrs2))
    | None, Variant_boxed _, Variant_unboxed ->
        Some (Unboxed_representation (Second, attrs_of_only cstrs1))
    | None, Variant_extensible, _ ->
      Some (Extensible_representation First)
    | None, _, Variant_extensible ->
      Some (Extensible_representation Second)
    | None, Variant_with_null, _ ->
      Some (With_null_representation First)
    | None, _, Variant_with_null ->
      Some (With_null_representation Second)
end

(* Inclusion between "private" annotations *)
let privacy_mismatch env decl1 decl2 =
  match decl1.type_private, decl2.type_private with
  | Private, Public -> begin
      match decl1.type_kind, decl2.type_kind with
      | Type_record  _, Type_record  _ -> Some Private_record_type
      | Type_record_unboxed_product  _, Type_record_unboxed_product  _ ->
          Some Private_record_unboxed_product_type
      | Type_variant _, Type_variant _ -> Some Private_variant_type
      | Type_open,      Type_open      -> Some Private_extensible_variant
      | Type_abstract _, Type_abstract _
        when Option.is_some decl2.type_manifest -> begin
          match decl1.type_manifest with
          | Some ty1 -> begin
            let ty1 = Ctype.expand_head env ty1 in
            match get_desc ty1 with
            | Tvariant row when Btype.is_constr_row ~allow_ident:true
                                  (row_more row) ->
                Some Private_row_type
            | Tobject (fi, _) when Btype.is_constr_row ~allow_ident:true
                                     (snd (Ctype.flatten_fields fi)) ->
                Some Private_row_type
            | _ ->
                Some Private_type_abbreviation
            end
          | None ->
              None
        end
      | _, _ ->
          None
    end
  | _, _ ->
      None

let private_variant env row1 row2 =
    let r1, r2, pairs =
      Ctype.merge_row_fields (row_fields row1) (row_fields row2)
    in
    let row1_closed = row_closed row1 in
    let row2_closed = row_closed row2 in
    let err =
      if row2_closed && not row1_closed then Some Only_outer_closed
      else begin
        match row2_closed, Ctype.filter_row_fields false r1 with
        | true, (s, _) :: _ ->
            Some (Missing (Second, s) : private_variant_mismatch)
        | _, _ -> None
      end
    in
    if err <> None then err else
    let err =
      let missing =
        List.find_opt
          (fun (_,f) ->
             match row_field_repr f with
             | Rabsent | Reither _ -> false
             | Rpresent _ -> true)
          r2
      in
      match missing with
      | None -> None
      | Some (s, _) -> Some (Missing (First, s) : private_variant_mismatch)
    in
    if err <> None then err else
    let rec loop tl1 tl2 pairs =
      match pairs with
      | [] -> begin
          match Ctype.equal env false tl1 tl2 with
          | exception Ctype.Equality err ->
              Some (Types err : private_variant_mismatch)
          | () -> None
        end
      | (s, f1, f2) :: pairs -> begin
          match row_field_repr f1, row_field_repr f2 with
          | Rpresent to1, Rpresent to2 -> begin
              match to1, to2 with
              | Some t1, Some t2 ->
                  loop (t1 :: tl1) (t2 :: tl2) pairs
              | None, None ->
                  loop tl1 tl2 pairs
              | Some _, None | None, Some _ ->
                  Some (Incompatible_types_for s)
            end
          | Rpresent to1, Reither(const2, ts2, _) -> begin
              match to1, const2, ts2 with
              | Some t1, false, [t2] -> loop (t1 :: tl1) (t2 :: tl2) pairs
              | None, true, [] -> loop tl1 tl2 pairs
              | _, _, _ -> Some (Incompatible_types_for s)
            end
          | Rpresent _, Rabsent ->
              Some (Missing (Second, s) : private_variant_mismatch)
          | Reither(const1, ts1, _), Reither(const2, ts2, _) ->
              if const1 = const2 && List.length ts1 = List.length ts2 then
                loop (ts1 @ tl1) (ts2 @ tl2) pairs
              else
                Some (Incompatible_types_for s)
          | Reither _, Rpresent _ ->
              Some (Presence s)
          | Reither _, Rabsent ->
              Some (Missing (Second, s) : private_variant_mismatch)
          | Rabsent, (Reither _ | Rabsent) ->
              loop tl1 tl2 pairs
          | Rabsent, Rpresent _ ->
              Some (Missing (First, s) : private_variant_mismatch)
        end
    in
    loop [] [] pairs

let private_object env fields1 fields2 =
  let pairs, _miss1, miss2 = Ctype.associate_fields fields1 fields2 in
  let err =
    match miss2 with
    | [] -> None
    | (f, _, _) :: _ -> Some (Missing f)
  in
  if err <> None then err else
  let tl1, tl2 =
    List.split (List.map (fun (_,_,t1,_,t2) -> t1, t2) pairs)
  in
  begin
    match Ctype.equal env false tl1 tl2 with
    | exception Ctype.Equality err -> Some (Types err)
    | () -> None
  end

let type_manifest env ty1 ty2 priv2 kind2 =
  let ty1' = Ctype.expand_head env ty1 and ty2' = Ctype.expand_head env ty2 in
  match get_desc ty1', get_desc ty2' with
  | Tvariant row1, Tvariant row2
    when is_absrow env (row_more row2) -> begin
      assert (Ctype.is_equal env false [ty1] [row_more row2]);
      match private_variant env row1 row2 with
      | None -> None
      | Some err -> Some (Private_variant(ty1, ty2, err))
    end
  | Tobject (fi1, _), Tobject (fi2, _)
    when is_absrow env (snd (Ctype.flatten_fields fi2)) -> begin
      let (fields2,rest2) = Ctype.flatten_fields fi2 in
      let (fields1,_) = Ctype.flatten_fields fi1 in
      assert (Ctype.is_equal env false [ty1] [rest2]);
      match private_object env fields1 fields2 with
      | None -> None
      | Some err -> Some (Private_object(ty1, ty2, err))
    end
  | _ -> begin
      let is_private_abbrev_2 =
        match priv2, kind2 with
        | Private, Type_abstract _ -> begin
            (* Same checks as the [when] guards from above, inverted *)
            match get_desc ty2' with
            | Tvariant row ->
                not (is_absrow env (row_more row))
            | Tobject (fi, _) ->
                not (is_absrow env (snd (Ctype.flatten_fields fi)))
            | _ -> true
          end
        | _, _ -> false
      in
      match
        if is_private_abbrev_2 then
          Ctype.equal_private env ty1 ty2
        else
          Ctype.equal env false [ty1] [ty2]
      with
      | exception Ctype.Equality err -> Some (Manifest err)
      | () -> None
    end

(* Note [Contravariance of type parameter jkinds]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   The goal of this function is to check whether every possible instantiation of
   decl2 is also a valid instantiation of decl1. Ideally, we would just make
   that check directly, by taking the type parameters of decl2, instantiating
   them with fresh constants (throughout the manifest/kind of decl2), and then
   unifying decl2 with an instance'd decl1. The problem with this approach is
   that it is not upstream-compatible: upstream requires that the constraints of
   decl1 and decl2 match exactly, even though this is more than is necessary for
   soundness.

   On the other hand, we really want to support contravariance of jkinds in type
   parameters, to allow something like this:

   {[
     module F (X : sig type ('a : value) t ... end) = ...
     module Array = struct type ('a : any) t ... end
     module M = F(Array)
   ]}

   This is perfectly safe -- [F] will use [X.t] only with [value]s, which is
   fine because [X.t] works with [any] type -- but requires contravariance.

   So this is our approach:

   1. Check the type parameters for equality (allowing renaming), but skipping
   any jkind checks. We must allow renaming because the decls will bind separate
   variables, and we skip the jkind checks precisely because we don't wish to
   require the jkinds be equal.

   2. Copy the two declarations. This allows us to unify parts of the copies
   without affecting the originals. We use [generic_instance_declaration]
   instead of [instance_declaration] because generic variables are not treated
   as weakly polymorphic by the pretty printer. That's the only reason to be
   generic here: levels simply don't matter in this bit.

   3. Rigidify the type parameters of decl2. We want to make sure any possible
   instantiation of decl2 is a legal instantiation of decl1, so if unification
   affects the type parameters of decl2, that's an error. (Alternative: we could
   reify instead of rigidify. But rigidify seems simpler, given that it does not
   need to extend the env.)

   (Perhaps surprisingly, rigidify does not actually make things rigid. Instead,
   it remembers the variables free in a type and then can check after a
   unification to see whether any of them changed. This check is Step 6 in this
   Note.)

   4. Unify the type parameters of decl1 and decl2. This unification can fail
   only by a jkind problem: anything else would have been caught by the equality
   check in Step 1. An outright jkind problem will raise [Unify], and this gets
   reported to the user. To format the error message well, we can safely search
   only for a [Bad_jkind] event; it has to be there somewhere. There also might
   be a jkind mismatch which causes the jkind of a variable in decl2 to be
   lowered; this gets caught by the call to [all_distinct_vars_with_original_jkinds]
   in Step 6.

   5. Check the type definitions. This includes doing all the checks on both the
   manifest and the kind. These checks use Ctype.equal. Importantly, this happens
   *after* unifying the type parameters, allowing an example like

   {[
     module M : sig
       type ('a : value) t = 'a
     end = struct
       type ('a : any) t = 'a
     end
   ]}

   A naive check that the jkind of the structure's [t] is less than that of the
   sig's [t] would fail: the structure's [t] has jkind [any] while the sig has
   jkind [value]. But it's actually all OK: because we have unified the parameters,
   the jkind of the struct will be [value], and then we'll check that [value]
   is a subjkind of [value], and the definition will be accepted.

   One worry might be about existential variables. Might they get unified? No.
   These, by definition, do not appear in the type parameters. And because all
   checks on the type definition bodies are done with [Ctype.equal], we know the
   existentials will remain untouched.

   6. Call [all_distinct_vars_with_original_jkinds]. This is the counterpart to
   Step 3 that checks that none of the type variables in decl2 needed to have
   their jkinds changed during unification.
   *)

(* See Note [Contravariance of type parameter jkinds]. *)
let type_declarations ?(equality = false) ~loc env ~mark name
      decl1 path decl2 =
  Builtin_attributes.check_alerts_inclusion
    ~def:decl1.type_loc
    ~use:decl2.type_loc
    loc
    decl1.type_attributes decl2.type_attributes
    name;
  if decl1.type_arity <> decl2.type_arity then Some Arity else
  (* Step 1 from the Note *)
  let err =
    match Ctype.equal ~do_jkind_check:false env true
            decl1.type_params decl2.type_params with
    | exception Ctype.Equality err -> Some (Constraint err)
    | () -> None
  in
  if err <> None then err else
  (* Step 2 from the Note *)
  let decl1 = Ctype.generic_instance_declaration decl1 in
  let decl2 = Ctype.generic_instance_declaration decl2 in
  (* Step 3 from the Note *)
  let rigidity_info = Ctype.Rigidify.rigidify_list decl2.type_params in
  (* Step 4 from the Note *)
  let err =
    match
      List.iter2 (Ctype.unify env) decl1.type_params decl2.type_params
    with
      | exception Ctype.Unify err ->
        let get_jkind_violation = function
          | Errortrace.Bad_jkind (ty, v) -> Some (Parameter_jkind (ty, v))
          | _ -> None
        in
        begin match List.find_map get_jkind_violation err.trace with
        | Some _ as err -> err
        | None -> Misc.fatal_errorf
                    "Unification in type_declarations failed, \
                     but not with Bad_jkind:@;<1 2>%t"
              (fun ppf -> Printtyp.report_unification_error ppf env err
               (fun ppf -> Format.fprintf ppf "The type")
               (fun ppf -> Format.fprintf ppf "does not unify with the type"))
        end
      | () -> None
  in
  if err <> None then err else
  (* Step 5 from the Note *)
  let err =
    match privacy_mismatch env decl1 decl2 with
    | Some err -> Some (Privacy err)
    | None -> None
  in
  if err <> None then err else
  let err = match (decl1.type_manifest, decl2.type_manifest) with
      (_, None) -> None
    | (Some ty1, Some ty2) ->
         type_manifest env ty1 ty2 decl2.type_private decl2.type_kind
    | (None, Some ty2) ->
        let ty1 =
          Btype.newgenty (Tconstr(path, decl2.type_params, ref Mnil))
        in
        match Ctype.equal env false [ty1] [ty2] with
        | exception Ctype.Equality err -> Some (Manifest err)
        | () -> None
  in
  if err <> None then err else
  let mark_and_compare_records record_form labels1 rep1 labels2 rep2 =
    if mark then begin
      let mark usage lbls =
        List.iter (Env.mark_label_used usage) lbls
      in
      let usage : Env.label_usage =
        if decl2.type_private = Public then Env.Exported
        else Env.Exported_private
      in
      mark usage labels1;
      if equality then mark Env.Exported labels2
    end;
    Record_diffing.compare_with_representation ~loc record_form env
      decl1.type_params decl2.type_params
      labels1 labels2
      rep1 rep2
  in
  let err = match (decl1.type_kind, decl2.type_kind) with
      (_, Type_abstract _) ->
        (* No need to check jkinds if decl2 has a manifest; we've already
           checked for type equality, above. Oddly, this is not just an
           optimization; unconditionally checking jkinds causes a failure
           around recursive modules (test case: shapes/recmodules.ml).
           Richard spent several hours trying to understand what was going
           on there (after the substitution in [Typemod.check_recmodule_inclusion],
           there was a type_declaration whose [type_jkind] didn't match its
           [type_manifest]), but just skipping this check when there is a
           manifest fixes the problem. *)
        if Option.is_none decl2.type_manifest then
          (* Note that [decl2.type_jkind] is an upper bound *)
          match Ctype.check_decl_jkind env decl1 decl2.type_jkind with
           | Ok _ -> None
           | Error v -> Some (Jkind v)
        else None
    | (Type_variant (cstrs1, rep1, umc1), Type_variant (cstrs2, rep2, umc2)) -> begin
        if mark then begin
          let mark usage cstrs =
            List.iter (Env.mark_constructor_used usage) cstrs
          in
          let usage : Env.constructor_usage =
            if decl2.type_private = Public then Env.Exported
            else Env.Exported_private
          in
          mark usage cstrs1;
          if equality then mark Env.Exported cstrs2
        end;
        Misc.Stdlib.Option.first_some
          (Variant_diffing.compare_with_representation ~loc env
              decl1.type_params
              decl2.type_params
              cstrs1
              cstrs2
              rep1
              rep2)
          (fun () -> compare_unsafe_mode_crossing ~env umc1 umc2)
      end
    | (Type_record(labels1,rep1,umc1), Type_record(labels2,rep2,umc2)) -> begin
        Misc.Stdlib.Option.first_some
          (mark_and_compare_records Legacy labels1 rep1 labels2 rep2)
          (fun () -> compare_unsafe_mode_crossing ~env umc1 umc2)
      end
    | (Type_record_unboxed_product(labels1,rep1,umc1),
       Type_record_unboxed_product(labels2,rep2,umc2)) -> begin
        Misc.Stdlib.Option.first_some
          (mark_and_compare_records Unboxed_product labels1 rep1 labels2 rep2)
          (fun () -> compare_unsafe_mode_crossing ~env umc1 umc2)
      end
    | (Type_open, Type_open) -> None
    | (_, _) -> Some (Kind (of_kind decl1.type_kind, of_kind decl2.type_kind))
  in
  if err <> None then err else
  (* Step 6 from the Note *)
  match Ctype.Rigidify.all_distinct_vars_with_original_jkinds env rigidity_info with
  | Unification_failure { name; ty }
    (* This should be caught by the call to Ctype.equal above *)
    -> Misc.fatal_errorf
         "Unification failure in type inclusion rigidity check:@;\
          %s unified with %a."
         (match name with None -> "_" | Some n -> "'" ^ n)
         Printtyp.type_expr ty
  | Jkind_mismatch { original_jkind; inferred_jkind; ty } ->
     let jkind_of_type ty = Some (Ctype.type_jkind_purely env ty) in
     Some (Parameter_jkind
             (ty, Jkind.Violation.of_ ~jkind_of_type
                    (Not_a_subjkind (Jkind.disallow_right original_jkind,
                                     Jkind.disallow_left inferred_jkind,
                                     []))))
  | All_good ->
  let abstr = Btype.type_kind_is_abstract decl2 && decl2.type_manifest = None in
  let need_variance =
    abstr || decl1.type_private = Private || decl1.type_kind = Type_open in
  if not need_variance then None else
  let abstr = abstr || decl2.type_private = Private in
  let opn = decl2.type_kind = Type_open && decl2.type_manifest = None in
  let constrained ty = not (Btype.is_Tvar ty) in
  if List.for_all2
      (fun ty (v1,v2) ->
        let open Variance in
        let imp a b = not a || b in
        let (co1,cn1) = get_upper v1 and (co2,cn2) = get_upper v2 in
        (if abstr then (imp co1 co2 && imp cn1 cn2)
         else if opn || constrained ty then (co1 = co2 && cn1 = cn2)
         else true) &&
        let (p1,n1,j1) = get_lower v1 and (p2,n2,j2) = get_lower v2 in
        imp abstr (imp p2 p1 && imp n2 n1 && imp j2 j1))
      decl2.type_params (List.combine decl1.type_variance decl2.type_variance)
  then None else Some Variance

(* Inclusion between extension constructors *)

let extension_constructors ~loc env ~mark id ext1 ext2 =
  if mark then begin
    let usage : Env.constructor_usage =
      if ext2.ext_private = Public then Env.Exported
      else Env.Exported_private
    in
    Env.mark_extension_used usage ext1
  end;
  let ty1 =
    Btype.newgenty (Tconstr(ext1.ext_type_path, ext1.ext_type_params, ref Mnil))
  in
  let ty2 =
    Btype.newgenty (Tconstr(ext2.ext_type_path, ext2.ext_type_params, ref Mnil))
  in
  let tl1 = ty1 :: ext1.ext_type_params in
  let tl2 = ty2 :: ext2.ext_type_params in
  match Ctype.equal env true tl1 tl2 with
  | exception Ctype.Equality err ->
      Some (Constructor_mismatch (id, ext1, ext2, Type err))
  | () ->
    let r =
      Variant_diffing.compare_constructors ~loc env
        ext1.ext_type_params ext2.ext_type_params
        ext1.ext_ret_type ext2.ext_ret_type
        ext1.ext_args ext2.ext_args
    in
    match r with
    | Some r -> Some (Constructor_mismatch (id, ext1, ext2, r))
    | None ->
      match ext1.ext_private, ext2.ext_private with
      | Private, Public -> Some Constructor_privacy
      | _, _ -> None
