open! Stdppx
open Ppxlib_ast.Asttypes
open Ppxlib_ast.Parsetree

module Modality = struct
  type nonrec t = modality = Modality of string [@@unboxed]
end

module Modalities = struct
  type t = Modality.t loc list

  let none = []
  let portable ~loc = [ { txt = Modality.Modality "portable"; loc } ]
end

module Mode = struct
  type t = mode = Mode of string [@@unboxed]
end

module Modes = struct
  type t = Mode.t loc list

  let none = []
  let local ~loc = [ { txt = Mode "local"; loc } ]
end

module Include_kind = struct
  type t = include_kind =
    | Structure
    | Functor
end

type arrow_argument =
  { arg_label : arg_label
  ; arg_modes : Modes.t
  ; arg_type : core_type
  }

type arrow_result =
  { result_modes : Modes.t
  ; result_type : core_type
  }

module Pcstr_tuple_arg = struct
  type t = constructor_argument

  let extract_modalities t = t.pca_modalities, t.pca_type
  let to_core_type t = t.pca_type

  let of_core_type core_type =
    { pca_type = core_type; pca_loc = core_type.ptyp_loc; pca_modalities = [] }
  ;;

  let map_core_type t ~f = { t with pca_type = f t.pca_type }

  let map_core_type_extra t ~f =
    let pca_type, extra = f t.pca_type in
    { t with pca_type }, extra
  ;;

  let create ~loc ~modalities ~type_ =
    { pca_type = type_; pca_loc = loc; pca_modalities = modalities }
  ;;
end

module Label_declaration = struct
  let extract_modalities ld = ld.pld_modalities, { ld with pld_modalities = [] }

  let create ~loc ~name ~mutable_ ~modalities ~type_ =
    { pld_loc = loc
    ; pld_modalities = modalities
    ; pld_name = name
    ; pld_type = type_
    ; pld_mutable = mutable_
    ; pld_attributes = []
    }
  ;;
end

module Value_description = struct
  let extract_modalities vd = vd.pval_modalities, { vd with pval_modalities = [] }

  let create ~loc ~name ~type_ ~modalities ~prim =
    { pval_loc = loc
    ; pval_modalities = modalities
    ; pval_name = name
    ; pval_type = type_
    ; pval_prim = prim
    ; pval_attributes = []
    }
  ;;
end

module Module_declaration = struct
  type t = module_declaration =
    { pmd_name : string option loc
    ; pmd_type : module_type
    ; pmd_modalities : modalities
    ; pmd_attributes : attributes
    ; pmd_loc : Location.t
    }

  let to_parsetree x = x
  let of_parsetree x = x
end

module Value_binding = struct
  let extract_modes vb = vb.pvb_modes, { vb with pvb_modes = [] }

  let create ~loc ~pat ~expr ~modes =
    { pvb_pat = pat
    ; pvb_expr = expr
    ; pvb_modes = modes
    ; pvb_attributes = []
    ; pvb_loc = loc
    }
  ;;
end

module Pexp_function = struct
  type nonrec function_param_desc = function_param_desc =
    | Pparam_val of arg_label * expression option * pattern
    | Pparam_newtype of string loc * jkind_annotation option

  type nonrec function_param = function_param =
    { pparam_loc : Location.t
    ; pparam_desc : function_param_desc
    }

  type nonrec type_constraint = type_constraint =
    | Pconstraint of core_type
    | Pcoerce of core_type option * core_type

  module Function_constraint = struct
    type t = function_constraint =
      { mode_annotations : Modes.t
      ; ret_mode_annotations : Modes.t
      ; ret_type_constraint : type_constraint option
      }

    let none =
      { mode_annotations = []; ret_mode_annotations = []; ret_type_constraint = None }
    ;;

    let is_none { ret_mode_annotations; ret_type_constraint; _ } =
      match ret_mode_annotations, ret_type_constraint with
      | [], None -> true
      | _, _ -> false
    ;;
  end

  type nonrec function_body = function_body =
    | Pfunction_body of expression
    | Pfunction_cases of case list * Location.t * attributes

  let to_parsetree ~params ~constraint_ ~body = Pexp_function (params, constraint_, body)

  let rec of_parsetree expr_desc ~loc =
    match expr_desc with
    | Pexp_function (a, b, c) -> Some (a, b, c)
    | Pexp_newtype (newtype, jkind_annotation, body) ->
      (match of_parsetree body.pexp_desc ~loc with
       | Some (a, b, c) ->
         let function_param : function_param =
           { pparam_loc = loc; pparam_desc = Pparam_newtype (newtype, jkind_annotation) }
         in
         Some (function_param :: a, b, c)
       | None -> None)
    | _ -> None
  ;;
end

type nonrec index_kind = index_kind =
  | Index_int
  | Index_unboxed_int64
  | Index_unboxed_int32
  | Index_unboxed_int16
  | Index_unboxed_int8
  | Index_unboxed_nativeint

type nonrec block_access = block_access =
  | Baccess_field of Longident.t loc
  | Baccess_array of mutable_flag * index_kind * expression
  | Baccess_block of mutable_flag * expression

type nonrec unboxed_access = unboxed_access = Uaccess_unboxed_field of Longident.t loc

module Core_type_desc = struct
  type t = core_type_desc =
    | Ptyp_any of jkind_annotation option
    | Ptyp_var of string * jkind_annotation option
    | Ptyp_arrow of arg_label * core_type * core_type * Modes.t * Modes.t
    | Ptyp_tuple of (string option * core_type) list
    | Ptyp_unboxed_tuple of (string option * core_type) list
    | Ptyp_constr of Longident.t loc * core_type list
    | Ptyp_object of object_field list * closed_flag
    | Ptyp_class of Longident.t loc * core_type list
    | Ptyp_alias of core_type * string loc option * jkind_annotation option
    | Ptyp_variant of row_field list * closed_flag * label list option
    | Ptyp_poly of (string loc * jkind_annotation option) list * core_type
    | Ptyp_package of package_type
    | Ptyp_quote of core_type
    | Ptyp_splice of core_type
    | Ptyp_of_kind of jkind_annotation
    | Ptyp_extension of extension

  let of_parsetree x = x
  let to_parsetree x = x
end

module Pattern_desc = struct
  type t = pattern_desc =
    | Ppat_any
    | Ppat_var of string loc
    | Ppat_alias of pattern * string loc
    | Ppat_constant of constant
    | Ppat_interval of constant * constant
    | Ppat_tuple of (string option * pattern) list * closed_flag
    | Ppat_unboxed_tuple of (string option * pattern) list * closed_flag
    | Ppat_construct of
        Longident.t loc * ((string loc * jkind_annotation option) list * pattern) option
    | Ppat_variant of label * pattern option
    | Ppat_record of (Longident.t loc * pattern) list * closed_flag
    | Ppat_record_unboxed_product of (Longident.t loc * pattern) list * closed_flag
    | Ppat_array of mutable_flag * pattern list
    | Ppat_or of pattern * pattern
    | Ppat_constraint of pattern * core_type option * Modes.t
    | Ppat_type of Longident.t loc
    | Ppat_lazy of pattern
    | Ppat_unpack of string option loc
    | Ppat_exception of pattern
    | Ppat_extension of extension
    | Ppat_open of Longident.t loc * pattern

  let of_parsetree x = x
  let to_parsetree ~loc:_ x = x
end

module Expression_desc = struct
  type t = expression_desc =
    | Pexp_ident of Longident.t loc
    | Pexp_constant of constant
    | Pexp_let of mutable_flag * rec_flag * value_binding list * expression
    | Pexp_function of
        Pexp_function.function_param list
        * Pexp_function.Function_constraint.t
        * Pexp_function.function_body
    | Pexp_apply of expression * (arg_label * expression) list
    | Pexp_match of expression * case list
    | Pexp_try of expression * case list
    | Pexp_tuple of (string option * expression) list
    | Pexp_unboxed_tuple of (string option * expression) list
    | Pexp_construct of Longident.t loc * expression option
    | Pexp_variant of label * expression option
    | Pexp_record of (Longident.t loc * expression) list * expression option
    | Pexp_record_unboxed_product of
        (Longident.t loc * expression) list * expression option
    | Pexp_field of expression * Longident.t loc
    | Pexp_unboxed_field of expression * Longident.t loc
    | Pexp_setfield of expression * Longident.t loc * expression
    | Pexp_array of mutable_flag * expression list
    | Pexp_idx of block_access * unboxed_access list
    | Pexp_ifthenelse of expression * expression * expression option
    | Pexp_sequence of expression * expression
    | Pexp_while of expression * expression
    | Pexp_for of pattern * expression * expression * direction_flag * expression
    | Pexp_constraint of expression * core_type option * Modes.t
    | Pexp_coerce of expression * core_type option * core_type
    | Pexp_send of expression * label loc
    | Pexp_new of Longident.t loc
    | Pexp_setvar of label loc * expression
    | Pexp_override of (label loc * expression) list
    | Pexp_letmodule of string option loc * module_expr * expression
    | Pexp_letexception of extension_constructor * expression
    | Pexp_assert of expression
    | Pexp_lazy of expression
    | Pexp_poly of expression * core_type option
    | Pexp_object of class_structure
    | Pexp_newtype of string loc * jkind_annotation option * expression
    | Pexp_pack of module_expr
    | Pexp_open of open_declaration * expression
    | Pexp_letop of letop
    | Pexp_extension of extension
    | Pexp_unreachable
    | Pexp_stack of expression
    | Pexp_comprehension of comprehension_expression
    | Pexp_overwrite of expression * expression
    | Pexp_quote of expression
    | Pexp_splice of expression
    | Pexp_hole

  (* The ignored [loc] argument is used in shim_upstream.ml. *)
  let of_parsetree x ~loc:_ = x
  let to_parsetree ~loc:_ x = x
end

module Core_type = struct
  type t = core_type =
    { ptyp_desc : Core_type_desc.t
    ; ptyp_loc : Location.t
    ; ptyp_loc_stack : Location.t list
    ; ptyp_attributes : attributes
    }

  let of_parsetree x = x
  let to_parsetree x = x
end

module Type_kind = struct
  type t = type_kind =
    | Ptype_abstract
    | Ptype_variant of constructor_declaration list
    | Ptype_record of label_declaration list
    | Ptype_record_unboxed_product of label_declaration list
    | Ptype_open

  let of_parsetree x = x
  let to_parsetree x = x
end

module Constructor_declaration = struct
  let extract_vars_with_jkind_annotations cd = cd.pcd_vars

  let create ~name ~vars ~args ~res ~loc =
    { pcd_name = name
    ; pcd_vars = vars
    ; pcd_args = args
    ; pcd_res = res
    ; pcd_loc = loc
    ; pcd_attributes = []
    }
  ;;
end

module Signature_item_desc = struct
  type t = signature_item_desc =
    | Psig_value of value_description
    | Psig_type of rec_flag * type_declaration list
    | Psig_typesubst of type_declaration list
    | Psig_typext of type_extension
    | Psig_exception of type_exception
    | Psig_module of module_declaration
    | Psig_modsubst of module_substitution
    | Psig_recmodule of module_declaration list
    | Psig_modtype of module_type_declaration
    | Psig_modtypesubst of module_type_declaration
    | Psig_open of open_description
    | Psig_include of include_description * modalities
    | Psig_class of class_description list
    | Psig_class_type of class_type_declaration list
    | Psig_attribute of attribute
    | Psig_extension of extension * attributes
    | Psig_kind_abbrev of string loc * jkind_annotation

  let of_parsetree x = x
  let to_parsetree x = x
end

type nonrec jkind_annotation_desc = jkind_annotation_desc =
  | Pjk_default
  | Pjk_abbreviation of string
  | Pjk_mod of jkind_annotation * Modes.t
  | Pjk_with of jkind_annotation * core_type * Modality.t loc list
  | Pjk_kind_of of core_type
  | Pjk_product of jkind_annotation list

type nonrec jkind_annotation = jkind_annotation =
  { pjkind_loc : Location.t
  ; pjkind_desc : jkind_annotation_desc
  }

module Type_declaration = struct
  type t = type_declaration =
    { ptype_name : string loc
    ; ptype_params : (core_type * (variance * injectivity)) list
    ; ptype_cstrs : (core_type * core_type * Location.t) list
    ; ptype_kind : type_kind
    ; ptype_private : private_flag
    ; ptype_manifest : core_type option
    ; ptype_attributes : attributes
    ; ptype_jkind_annotation : jkind_annotation option
    ; ptype_loc : Location.t
    }

  let to_parsetree t = t
  let of_parsetree t = t
  let extract_jkind_annotation (td : type_declaration) = td.ptype_jkind_annotation
end

module Constant = struct
  type t = constant =
    | Pconst_integer of string * char option
    | Pconst_unboxed_integer of string * char
    | Pconst_char of char
    | Pconst_untagged_char of char
    | Pconst_string of string * Location.t * string option
    | Pconst_float of string * char option
    | Pconst_unboxed_float of string * char option

  let of_parsetree x = x
  let to_parsetree x = x
end

module Include_infos = struct
  type 'a t = 'a include_infos =
    { pincl_kind : Include_kind.t
    ; pincl_mod : 'a
    ; pincl_loc : Location.t
    ; pincl_attributes : attributes
    }

  let of_parsetree x = x
  let to_parsetree x = x
end

module Signature = struct
  type t = signature =
    { psg_modalities : Modalities.t
    ; psg_items : signature_item list
    ; psg_loc : Location.t
    }

  let of_parsetree x = x
  let to_parsetree x = x
end

module Structure_item_desc = struct
  type t = structure_item_desc =
    | Pstr_eval of expression * attributes
    | Pstr_value of rec_flag * value_binding list
    | Pstr_primitive of value_description
    | Pstr_type of rec_flag * type_declaration list
    | Pstr_typext of type_extension
    | Pstr_exception of type_exception
    | Pstr_module of module_binding
    | Pstr_recmodule of module_binding list
    | Pstr_modtype of module_type_declaration
    | Pstr_open of open_declaration
    | Pstr_class of class_declaration list
    | Pstr_class_type of class_type_declaration list
    | Pstr_include of include_declaration
    | Pstr_attribute of attribute
    | Pstr_extension of extension * attributes
    | Pstr_kind_abbrev of string loc * jkind_annotation

  let of_parsetree x = x
  let to_parsetree x = x
end

module Functor_parameter = struct
  type t = functor_parameter =
    | Unit (** [()] *)
    | Named of string option loc * module_type * Modes.t
    (** [Named(name, MT)] represents:
        - [(X : MT @@ modes)] when [name] is [Some X],
        - [(_ : MT @@ modes)] when [name] is [None] *)

  let to_parsetree x = x
  let of_parsetree x = x
end

module Module_type_desc = struct
  type t = module_type_desc =
    | Pmty_ident of Longident.t loc
    | Pmty_signature of signature
    | Pmty_functor of functor_parameter * module_type * Modes.t
    | Pmty_with of module_type * with_constraint list
    | Pmty_typeof of module_expr
    | Pmty_extension of extension
    | Pmty_alias of Longident.t loc
    | Pmty_strengthen of module_type * Longident.t loc

  let of_parsetree x = x
  let to_parsetree ~loc:_ x = x
end

module Module_expr_desc = struct
  type t = module_expr_desc =
    | Pmod_ident of Longident.t loc
    | Pmod_structure of structure
    | Pmod_functor of functor_parameter * module_expr
    | Pmod_apply of module_expr * module_expr
    | Pmod_constraint of module_expr * module_type option * Modes.t
    | Pmod_unpack of expression
    | Pmod_extension of extension
    | Pmod_instance of module_instance

  let of_parsetree x = x
  let to_parsetree ~loc:_ x = x
end

module Ast_traverse = struct
  class virtual map = Ppxlib_ast.Ast.map
  class virtual iter = Ppxlib_ast.Ast.iter
  class virtual ['acc] fold = ['acc] Ppxlib_ast.Ast.fold
  class virtual ['acc] fold_map = ['acc] Ppxlib_ast.Ast.fold_map
  class virtual ['ctx] map_with_context = ['ctx] Ppxlib_ast.Ast.map_with_context
  class virtual ['res] lift = ['res] Ppxlib_ast.Ast.lift

  class virtual ['ctx, 'res] lift_map_with_context =
    ['ctx, 'res] Ppxlib_ast.Ast.lift_map_with_context
end
