module Ext_name : sig
  val pstr_primitive_alias : string
  val psig_primitive_alias : string
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
end
