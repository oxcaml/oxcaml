open Astlib
open Ppxlib_ast.Asttypes
open Ppxlib_ast.Parsetree

(** This file can have a different implementation in the Jane Street experimental compiler
    and the upstream compiler, allowing ppxes to easily work with both versions *)

(** When you change one of [shim.{ml,mli}], please also consider if you need to make
    changes to the corresponding [shim_upstream.{ml,mli}]. A diff between the mli files is
    automatically generated at [shim.mli.diff], so you'll find out if you change one but
    not the other *)

module Mode : sig
  (** The modes that can go on function arguments or return types *)
  type t = mode = Mode of string [@@unboxed]
end

module Modes : sig
  type t = Mode.t loc list

  val none : t
  val local : loc:Location.t -> t
end

module Modality : sig
  (** The modalities that can go on constructor fields *)
  type t = modality = Modality of string [@@unboxed]
end

module Modalities : sig
  type t = Modality.t loc list

  val none : t
  val portable : loc:Location.t -> t
end

module Include_kind : sig
  type t = include_kind =
    | Structure
    | Functor
end

(** Function arguments; a value of this type represents:
    - [arg_mode arg_type -> ...] when [arg_label] is {{!Asttypes.arg_label.Nolabel}
      [Nolabel]},
    - [l:arg_mode arg_type -> ...] when [arg_label] is {{!Asttypes.arg_label.Labelled}
      [Labelled]}, and
    - [?l:arg_mode arg_type -> ...] when [arg_label] is {{!Asttypes.arg_label.Optional}
      [Optional]}. *)
type arrow_argument =
  { arg_label : arg_label
  ; arg_modes : Modes.t
  ; arg_type : core_type
  }

(** Function return types; a value of this type represents
    [... -> result_mode result_type]. *)
type arrow_result =
  { result_modes : Modes.t
  ; result_type : core_type
  }

(** A list of this type is stored in the [Pcstr_tuple] constructor of
    [constructor_arguments]. With JS extensions, fields in constructors can contain
    modalities. *)
module Pcstr_tuple_arg : sig
  type t = constructor_argument

  val extract_modalities : t -> Modalities.t * core_type
  val to_core_type : t -> core_type
  val of_core_type : core_type -> t
  val map_core_type : t -> f:(core_type -> core_type) -> t
  val map_core_type_extra : t -> f:(core_type -> core_type * 'a) -> t * 'a

  (** [loc] is ignored if there is no modality. *)
  val create : loc:Location.t -> modalities:Modalities.t -> type_:core_type -> t
end

(** This is an interface around the [Parsetree.label_declaration] type, describing one
    label in a record declaration. *)
module Label_declaration : sig
  val extract_modalities : label_declaration -> Modalities.t * label_declaration

  val create
    :  loc:Location.t
    -> name:string Location.loc
    -> mutable_:mutable_flag
    -> modalities:Modalities.t
    -> type_:core_type
    -> label_declaration
end

module Value_description : sig
  val extract_modalities : value_description -> Modalities.t * value_description

  val create
    :  loc:Location.t
    -> name:string Location.loc
    -> type_:core_type
    -> modalities:Modalities.t
    -> prim:string list
    -> value_description
end

module Module_declaration : sig
  type t = module_declaration =
    { pmd_name : string option loc
    ; pmd_type : module_type
    ; pmd_modalities : Modalities.t
    ; pmd_attributes : attributes (** [... [\@\@id1] [\@\@id2]] *)
    ; pmd_loc : Location.t
    }

  val to_parsetree : t -> module_declaration
  val of_parsetree : module_declaration -> t
end

module Value_binding : sig
  val extract_modes : value_binding -> Modes.t * value_binding

  val create
    :  loc:Location.t
    -> pat:pattern
    -> expr:expression
    -> modes:Modes.t
    -> value_binding
end

type nonrec jkind_annotation_desc = jkind_annotation_desc =
  | Pjk_default
  | Pjk_abbreviation of string
  | Pjk_mod of jkind_annotation * Modes.t
  | Pjk_with of jkind_annotation * core_type * Modalities.t
  | Pjk_kind_of of core_type
  | Pjk_product of jkind_annotation list

type nonrec jkind_annotation = jkind_annotation =
  { pjkind_loc : Location.t
  ; pjkind_desc : jkind_annotation_desc
  }

module Type_declaration : sig
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

  val of_parsetree : type_declaration -> t
  val to_parsetree : t -> type_declaration
  val extract_jkind_annotation : type_declaration -> jkind_annotation option
end

module Constant : sig
  type t = constant =
    | Pconst_integer of string * char option
    | Pconst_unboxed_integer of string * char
    | Pconst_char of char
    | Pconst_untagged_char of char
    | Pconst_string of string * Location.t * string option
    | Pconst_float of string * char option
    | Pconst_unboxed_float of string * char option

  val of_parsetree : constant -> t
  val to_parsetree : t -> constant
end

(** Match and construct [Pexp_function], as in the OCaml parsetree at or after 5.2. *)
module Pexp_function : sig
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

  module Function_constraint : sig
    type t = function_constraint =
      { mode_annotations : Modes.t
      ; ret_mode_annotations : Modes.t
      ; ret_type_constraint : type_constraint option
      }

    val none : t
    val is_none : t -> bool
  end

  type nonrec function_body = function_body =
    | Pfunction_body of expression
    | Pfunction_cases of case list * Location.t * attributes

  val to_parsetree
    :  params:function_param list
    -> constraint_:Function_constraint.t
    -> body:function_body
    -> expression_desc

  val of_parsetree
    :  expression_desc
    -> loc:Location.t
    -> (function_param list * Function_constraint.t * function_body) option
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

module Core_type_desc : sig
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

  val of_parsetree : core_type_desc -> t
  val to_parsetree : t -> core_type_desc
end

module Core_type : sig
  type t = core_type =
    { ptyp_desc : Core_type_desc.t
    ; ptyp_loc : Location.t
    ; ptyp_loc_stack : Location.t list
    ; ptyp_attributes : attributes
    }

  val of_parsetree : core_type -> t
  val to_parsetree : t -> core_type
end

module Pattern_desc : sig
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

  val of_parsetree : pattern_desc -> t
  val to_parsetree : loc:Location.t -> t -> pattern_desc
end

module Expression_desc : sig
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

  val of_parsetree : expression_desc -> loc:Location.t -> t
  val to_parsetree : loc:Location.t -> t -> expression_desc
end

module Type_kind : sig
  type t = type_kind =
    | Ptype_abstract
    | Ptype_variant of constructor_declaration list
    | Ptype_record of label_declaration list
    | Ptype_record_unboxed_product of label_declaration list
    | Ptype_open

  val of_parsetree : type_kind -> t
  val to_parsetree : t -> type_kind
end

module Constructor_declaration : sig
  val extract_vars_with_jkind_annotations
    :  constructor_declaration
    -> (string loc * jkind_annotation option) list

  val create
    :  name:string loc
    -> vars:(string loc * jkind_annotation option) list
    -> args:constructor_arguments
    -> res:core_type option
    -> loc:Location.t
    -> constructor_declaration
end

module Include_infos : sig
  type 'a t = 'a include_infos =
    { pincl_kind : Include_kind.t
    ; pincl_mod : 'a
    ; pincl_loc : Location.t
    ; pincl_attributes : attributes
    }

  val of_parsetree : 'a include_infos -> 'a t
  val to_parsetree : 'a t -> 'a include_infos
end

module Signature_item_desc : sig
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
    | Psig_include of include_description * Modalities.t
    | Psig_class of class_description list
    | Psig_class_type of class_type_declaration list
    | Psig_attribute of attribute
    | Psig_extension of extension * attributes
    | Psig_kind_abbrev of string loc * jkind_annotation

  val of_parsetree : signature_item_desc -> t
  val to_parsetree : t -> signature_item_desc
end

module Signature : sig
  type t = signature =
    { psg_modalities : Modalities.t
    ; psg_items : signature_item list
    ; psg_loc : Location.t
    }

  val of_parsetree : signature -> t
  val to_parsetree : t -> signature
end

module Structure_item_desc : sig
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

  val of_parsetree : structure_item_desc -> t
  val to_parsetree : t -> structure_item_desc
end

module Functor_parameter : sig
  type t = functor_parameter =
    | Unit
    | Named of string option loc * module_type * Modes.t

  val to_parsetree : t -> functor_parameter
  val of_parsetree : functor_parameter -> t
end

module Module_type_desc : sig
  type t = module_type_desc =
    | Pmty_ident of Longident.t loc
    | Pmty_signature of signature
    | Pmty_functor of functor_parameter * module_type * Modes.t
    | Pmty_with of module_type * with_constraint list
    | Pmty_typeof of module_expr
    | Pmty_extension of extension
    | Pmty_alias of Longident.t loc
    | Pmty_strengthen of module_type * Longident.t loc

  val of_parsetree : module_type_desc -> t
  val to_parsetree : loc:Location.t -> t -> module_type_desc
end

module Module_expr_desc : sig
  type t = module_expr_desc =
    | Pmod_ident of Longident.t loc
    | Pmod_structure of structure
    | Pmod_functor of functor_parameter * module_expr
    | Pmod_apply of module_expr * module_expr
    | Pmod_constraint of module_expr * module_type option * Modes.t
    | Pmod_unpack of expression
    | Pmod_extension of extension
    | Pmod_instance of module_instance

  val of_parsetree : module_expr_desc -> t
  val to_parsetree : loc:Location.t -> t -> module_expr_desc
end

module Ast_traverse : sig
  class virtual map : Ppxlib_ast.Ast.map
  class virtual iter : Ppxlib_ast.Ast.iter
  class virtual ['acc] fold : ['acc] Ppxlib_ast.Ast.fold
  class virtual ['acc] fold_map : ['acc] Ppxlib_ast.Ast.fold_map
  class virtual ['ctx] map_with_context : ['ctx] Ppxlib_ast.Ast.map_with_context
  class virtual ['res] lift : ['res] Ppxlib_ast.Ast.lift

  class virtual ['ctx, 'res] lift_map_with_context :
    ['ctx, 'res] Ppxlib_ast.Ast.lift_map_with_context
end
