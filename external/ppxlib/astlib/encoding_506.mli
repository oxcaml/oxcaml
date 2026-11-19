module Ext_name : sig
  val pstr_primitive_alias : string
  val psig_primitive_alias : string
  val pexp_hole : string
  val pmod_hole : string
end

module To_505 : sig
  open Ast_505
  open Asttypes
  open Parsetree

  val encode_psig_primitive_alias :
    loc:Location.t ->
    string loc ->
    core_type option ->
    Longident.t loc ->
    attributes ->
    signature_item_desc

  val decode_psig_primitive_alias :
    loc:Location.t ->
    payload ->
    attributes ->
    string loc * core_type option * Longident.t loc * attributes

  val encode_pstr_primitive_alias :
    loc:Location.t ->
    string loc ->
    core_type option ->
    Longident.t loc ->
    attributes ->
    structure_item_desc

  val decode_pstr_primitive_alias :
    loc:Location.t ->
    payload ->
    attributes ->
    string loc * core_type option * Longident.t loc * attributes

  val encode_pexp_hole : loc:Location.t -> expression_desc
  val encode_pmod_hole : loc:Location.t -> module_expr_desc
end

module To_502 : sig
  open Ast_502
  open Asttypes
  open Parsetree

  val encode_pexp_hole : loc:Location.t -> expression_desc
  val encode_pmod_hole : loc:Location.t -> module_expr_desc
end
