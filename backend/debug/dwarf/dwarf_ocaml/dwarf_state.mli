(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** State that is shared amongst the various dwarf_* modules. *)

open Asm_targets
open Dwarf_low
open Dwarf_high
module RS = Runtime_shape

type function_range =
  { start_label : Asm_label.t;
    end_label : Asm_label.t;
    offset_past_end_label : int option;
    function_symbol : Asm_symbol.t
  }

type code_layout =
  | Continuous_code_section of
      { code_begin : Asm_symbol.t;
        code_end : Asm_symbol.t
      }
  | Function_sections

module Diagnostics : sig
  type variable_reduction =
    { shape_size_before_reduction_in_bytes : int;
      shape_size_after_reduction_in_bytes : int;
      shape_size_after_evaluation_in_bytes : int;
      reduction_steps : int;
      evaluation_steps : int;
      type_name : string;
      type_layout : Jkind_types.Sort.Const.t;
      dwarf_die_size : int;
      cms_files_loaded : int;
      cms_files_cached : int;
      cms_files_missing : string list;
      cms_files_unreadable : string list
    }

  type t = { mutable variables : variable_reduction list }
end

(** Context threaded through the translation of runtime shapes to DWARF DIEs. *)
module Die_gen_ctx : sig
  (** The recursive-variable environment used when translating runtime shapes to
      DWARF DIEs. *)
  module Rec_var_env : sig
    type t

    include Hashtbl.HashedType with type t := t

    val get_opt :
      t -> de_bruijn_index:RS.DeBruijn_index.t -> Proto_die.reference option
  end

  (** Cache memoizing [runtime_shape_to_dwarf_die] results. *)
  module Cache : sig
    type t

    val create : initial_size:int -> t

    val find :
      t -> inp:RS.t -> rec_env:Rec_var_env.t -> Proto_die.reference option

    val add :
      t -> inp:RS.t -> rec_env:Rec_var_env.t -> outp:Proto_die.reference -> unit
  end

  (** DWARF DIE cache for named type shapes. *)
  module Name_cache : sig
    type t

    val create : initial_size:int -> t

    (** [find_unused_name_or_cached t name runtime_shape] returns
        [Left reference] if a (possibly suffix-numbered) version of [name] is
        already associated with [runtime_shape], or [Right name'] with the first
        unused suffix-numbered version of [name] otherwise. *)
    val find_unused_name_or_cached :
      t -> string -> RS.t -> (Proto_die.reference, string) Either.t

    val add : t -> string -> RS.t -> Proto_die.reference -> unit
  end

  type t

  val create : initial_size:int -> t

  val cache : t -> Cache.t

  val name_cache : t -> Name_cache.t

  (** The empty recursive-variable environment, interned in this context's table
      so that it can be used as part of a cache key. *)
  val empty_rec_env : t -> Rec_var_env.t

  (** [push_rec_binder t rec_env ref] extends [rec_env] with a binding for
      [ref]. The result is interned in this context's table for use as part of a
      cache key. *)
  val push_rec_binder :
    t -> Rec_var_env.t -> Proto_die.reference -> Rec_var_env.t
end

type t

val create :
  compilation_unit_header_label:Asm_label.t ->
  compilation_unit_proto_die:Proto_die.t ->
  code_layout:code_layout ->
  Debug_loc_table.t ->
  Debug_ranges_table.t ->
  Address_table.t ->
  Location_list_table.t ->
  get_file_num:(string -> int) ->
  sourcefile:string ->
  t

val compilation_unit_header_label : t -> Asm_label.t

val compilation_unit_proto_die : t -> Proto_die.t

val debug_loc_table : t -> Debug_loc_table.t

val debug_ranges_table : t -> Debug_ranges_table.t

val address_table : t -> Address_table.t

val location_list_table : t -> Location_list_table.t

val function_abstract_instances :
  t -> (Proto_die.t * Asm_symbol.t) Asm_symbol.Tbl.t

val can_reference_dies_across_units : t -> bool

val get_file_num : t -> string -> int

val sourcefile : t -> string

val diagnostics : t -> Diagnostics.t

val complex_shape_cache : t -> Complex_shape.Shape_cache.t

val eval_context : t -> Type_shape.Evaluated_shape.Eval_context.t

val die_gen_ctx : t -> Die_gen_ctx.t

val add_variable_reduction_diagnostic :
  t -> Diagnostics.variable_reduction -> unit

val code_layout : t -> code_layout

val function_ranges : t -> function_range list

val record_function_range :
  t ->
  function_symbol:Asm_symbol.t ->
  start_label:Asm_label.t ->
  end_label:Asm_label.t ->
  offset_past_end_label:int option ->
  unit

module Debug : sig
  val log : ('a, Format.formatter, unit) format -> 'a
end
