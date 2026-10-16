module Ext_name : sig
  val pexp_struct_item : string
  val ptyp_functor : string
  val preserve_ppat_constraint : string
  val ptype_kind_external : string
  val external_psig : string
  val external_pstr_type : string
  val external_pmty_with : string
end

module To_504 : sig
  open Ast_504.Asttypes
  open Ast_504.Parsetree

  val encode_pexp_struct_item :
    loc:Location.t -> structure_item * expression -> expression_desc

  val decode_pexp_struct_item :
    loc:Location.t -> payload -> structure_item * expression

  val encode_ptyp_functor :
    loc:Location.t ->
    arg_label * string loc * package_type * core_type ->
    core_type_desc

  val decode_ptyp_functor :
    loc:Location.t ->
    payload ->
    arg_label * string loc * package_type * core_type

  val encode_ptype_kind_external :
    loc:Location.t -> string -> attributes -> type_kind * attributes

  val decode_ptype_kind_external :
    type_declaration -> (string * attributes) option

  val encode_external_psig_type :
    loc:Location.t -> rec_flag -> type_declaration list -> signature_item_desc

  val encode_external_psig_typesubst :
    loc:Location.t -> type_declaration list -> signature_item_desc

  val decode_external_psig :
    loc:Location.t -> payload -> attributes -> signature_item_desc

  val encode_external_pstr_type :
    loc:Location.t -> rec_flag -> type_declaration list -> structure_item_desc

  val decode_external_pstr_type :
    loc:Location.t -> payload -> attributes -> structure_item_desc

  val encode_external_pmty_with :
    loc:Location.t -> module_type -> with_constraint list -> module_type_desc

  val decode_external_pmty_with : loc:Location.t -> payload -> module_type_desc

  val must_preserve_ppat_constraint : attributes -> attributes option
  (** Returns [None] if the list does not contain
      [@ppxlib.migration.preserve_ppat_constraint_505] or [Some l] where [l] is
      the remainder of the attributes. Should be used to determine whether a
      [Ppat_constraint (Ppat_unpack m, Ptyp_package s)] node should be preserved
      when migrated to 5.5 or turned into [Ppat_unpack (m, Some s)]. The
      attribute should be attached to the [Ppat_unpack] node within the
      [Ppat_constraint]. *)

  val preserve_ppat_constraint : pattern -> core_type -> pattern_desc
end
