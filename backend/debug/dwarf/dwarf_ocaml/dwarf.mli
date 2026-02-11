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

(** Generation and emission of DWARF debugging information for OCaml compilation
    units. *)

open Asm_targets

type t

(** Create a value of type [t], which holds all state necessary to emit DWARF
    debugging information for a single compilation unit. The names of certain
    parameters line up with the code in [Asmgen]. The current [Compilation_unit]
    must have been set before calling this function. *)
val create :
  sourcefile:string ->
  unit_name:Ident.t ->
  asm_directives:Asm_directives_dwarf.t ->
  get_file_id:(string -> int) ->
  code_layout:Dwarf_state.code_layout ->
  t

(** Record a function's address range. This function can be called
    unconditionally; it will only record the range when [code_layout] is
    [Function_sections]. *)
val record_function_range :
  t ->
  function_symbol:Asm_symbol.t ->
  start_label:Asm_label.t ->
  end_label:Asm_label.t ->
  offset_past_end_label:int option ->
  unit

type fundecl = private
  { fun_end_label : Cmm.label;
    fundecl : Linear.fundecl
  }

val dwarf_for_fundecl :
  t ->
  Linear.fundecl ->
  fun_end_label:Cmm.label ->
  ppf_dump:Format.formatter ->
  fundecl

(** Write the DWARF information to the assembly file. This should only be called
    once all (in)constants and function declarations have been passed to the
    above functions. *)
val emit : t -> binary_backend_available:bool -> unit

val emit_delayed : t -> binary_backend_available:bool -> unit
