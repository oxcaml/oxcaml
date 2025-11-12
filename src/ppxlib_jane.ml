module Shim = Shim
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
type mode = Shim.Mode.t = Mode of string [@@unboxed]
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
  | Pjk_abbreviation of string
  | Pjk_mod of Shim.jkind_annotation * Shim.Modes.t
  | Pjk_with of Shim.jkind_annotation * Ppxlib_ast.Parsetree.core_type * Shim.Modalities.t
  | Pjk_kind_of of Ppxlib_ast.Parsetree.core_type
  | Pjk_product of Shim.jkind_annotation list

type jkind_annotation = Shim.jkind_annotation =
  { pjkind_loc : Location.t
  ; pjkind_desc : Shim.jkind_annotation_desc
  }
