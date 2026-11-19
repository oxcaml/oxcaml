module Shim = Shim
module Longident = Shim.Longident
module Ast_builder = Ast_builder
module Ast_traverse = Ast_traverse
module Legacy_pexp_function = Legacy_pexp_function
module Language_feature_name = Names.Language_feature_name
module Constructor_name = Names.Constructor_name

(** {2 Common helper functions} *)
include Common

(** {2 Common Jane Street helper types} *)

type modality = Shim.Modality.t = Modality of string [@@unboxed]
type modalities = Shim.Modalities.t

type mode = Shim.Mode.t =
  | Pmode of string
  | Pmode_var of string
  | Pmode_bounded of mode_bounded

and mode_bounded = Shim.Mode.bounded =
  { pmode_upper : mode_bound
  ; pmode_lower : mode_bound
  }

and mode_bound = mode_bound_elem list

and mode_bound_elem = Shim.Mode.bound_elem =
  | Pbound_var of mode_bound_var
  | Pbound_const of string Location.loc list

and mode_bound_var = Shim.Mode.bound_var =
  { pbound_morph : string Location.loc option
  ; pbound_var : string Location.loc
  ; pbound_mod : string Location.loc list
  }

type modes = Shim.Modes.t

type arrow_result = Shim.arrow_result =
  { result_modes : modes
  ; result_type : Ppxlib_ast.Parsetree.core_type
  }

type arrow_argument = Shim.arrow_argument =
  { arg_label : Ppxlib_ast.Asttypes.arg_label
  ; arg_modes : modes
  ; arg_type : Ppxlib_ast.Parsetree.core_type
  }

type jkind_annotation_desc = Shim.jkind_annotation_desc =
  | Pjk_default
  | Pjk_abbreviation of Astlib.Longident.t Ppxlib_ast.Asttypes.loc
  | Pjk_operator of Shim.jkind_annotation * string Location.loc list
  | Pjk_mod of Shim.jkind_annotation * Shim.Modes.t
  | Pjk_with of Shim.jkind_annotation * Ppxlib_ast.Parsetree.core_type * Shim.Modalities.t
  | Pjk_kind_of of Ppxlib_ast.Parsetree.core_type
  | Pjk_product of Shim.jkind_annotation list

type jkind_annotation = Shim.jkind_annotation =
  { pjka_loc : Location.t
  ; pjka_desc : Shim.jkind_annotation_desc
  }
