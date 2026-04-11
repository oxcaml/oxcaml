module Ext_name : sig
  val ptyp_labeled_tuple : string
  val pexp_labeled_tuple : string
  val ppat_labeled_tuple : string
  val bivariant_pstr : string
  val bivariant_psig : string
  val bivariant_pmty_with : string
end

module To_503 : sig
  open Ast_503.Asttypes
  open Ast_503.Parsetree

  val encode_ptyp_labeled_tuple :
    loc:Location.t -> (string option * core_type) list -> core_type_desc

  val decode_ptyp_labeled_tuple :
    loc:Location.t -> payload -> (string option * core_type) list

  val encode_pexp_labeled_tuple :
    loc:Location.t -> (string option * expression) list -> expression_desc

  val decode_pexp_labeled_tuple :
    loc:Location.t -> payload -> (string option * expression) list

  val encode_ppat_labeled_tuple :
    loc:Location.t ->
    (string option * pattern) list ->
    closed_flag ->
    pattern_desc

  val decode_ppat_labeled_tuple :
    loc:Location.t -> payload -> (string option * pattern) list * closed_flag

  val encode_bivariant_param :
    core_type -> injectivity -> core_type * (variance * injectivity)

  val decode_bivariant_param :
    core_type * (variance * injectivity) -> (core_type * injectivity) option

  val encode_bivariant_pstr_type :
    loc:Location.t -> rec_flag -> type_declaration list -> structure_item_desc

  val encode_bivariant_pstr_typext :
    loc:Location.t -> type_extension -> structure_item_desc

  val encode_bivariant_pstr_class :
    loc:Location.t -> class_declaration list -> structure_item_desc

  val encode_bivariant_pstr_class_type :
    loc:Location.t -> class_type_declaration list -> structure_item_desc

  val encode_bivariant_psig_type :
    loc:Location.t -> rec_flag -> type_declaration list -> signature_item_desc

  val encode_bivariant_psig_typesubst :
    loc:Location.t -> type_declaration list -> signature_item_desc

  val encode_bivariant_psig_typext :
    loc:Location.t -> type_extension -> signature_item_desc

  val encode_bivariant_psig_class :
    loc:Location.t -> class_description list -> signature_item_desc

  val encode_bivariant_psig_class_type :
    loc:Location.t -> class_type_declaration list -> signature_item_desc

  val encode_bivariant_pmty_with :
    loc:Location.t -> module_type -> with_constraint list -> module_type_desc

  val decode_bivariant_pstr :
    loc:Location.t -> payload -> attributes -> structure_item_desc

  val decode_bivariant_psig :
    loc:Location.t -> payload -> attributes -> signature_item_desc

  val decode_bivariant_pmty_with : loc:Location.t -> payload -> module_type_desc
end

module To_502 : sig
  open Ast_502.Asttypes
  open Ast_502.Parsetree

  val encode_ptyp_labeled_tuple :
    loc:Location.t -> (string option * core_type) list -> core_type_desc

  val decode_ptyp_labeled_tuple :
    loc:Location.t -> payload -> (string option * core_type) list

  val encode_pexp_labeled_tuple :
    loc:Location.t -> (string option * expression) list -> expression_desc

  val decode_pexp_labeled_tuple :
    loc:Location.t -> payload -> (string option * expression) list

  val encode_ppat_labeled_tuple :
    loc:Location.t ->
    (string option * pattern) list ->
    closed_flag ->
    pattern_desc

  val decode_ppat_labeled_tuple :
    loc:Location.t -> payload -> (string option * pattern) list * closed_flag
end
