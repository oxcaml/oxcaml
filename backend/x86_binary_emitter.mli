(***********************************************************************)
(*                                                                     *)
(*                              OCaml                                  *)
(*                                                                     *)
(*  Copyright 2014, OCamlPro. All rights reserved.                     *)
(*  All rights reserved. This file is distributed under the terms of   *)
(*  the GNU Lesser General Public License version 2.1                  *)
(*                                                                     *)
(***********************************************************************)
(*
  Contributors:
  * Fabrice LE FESSANT (INRIA/OCamlPro)
*)

[@@@ocaml.warning "+a-40-41-42"]

open X86_ast
module String = Misc.Stdlib.String

type section =
  { sec_name : X86_proc.Section_name.t;
    mutable sec_instrs : asm_line array
  }

type data_size = B8 | B16 | B32 | B64

type symbol_binding = Sy_local | Sy_global | Sy_weak

type symbol = {
  sy_name : string;
  mutable sy_type : Asm_targets.Asm_directives.symbol_type option;
  mutable sy_size : int option;
  mutable sy_binding : symbol_binding;
  mutable sy_protected : bool;
  mutable sy_sec : section;
  mutable sy_pos : int option;
  mutable sy_num : int option; (* position in .symtab *)
}

module Relocation : sig
  module Kind : sig
    type t =
      (* 32 bits offset usually in data section *)
      | REL32 of string * int64
      | DIR32 of string * int64
      | DIR64 of string * int64
  end

  type t = { offset_from_section_beginning : int; kind : Kind.t }
end

module StringMap : Map.S with type key = string

(** Output of [assemble_section]. Cannot be consumed directly; must be
    passed through [resolve_global_patches] first. This type-level
    distinction ensures callers cannot forget the global-resolution
    pass, which folds same-section symbol differences to literals and
    emits ELF relocations for external references and supported
    cross-section PC-relative references. *)
type unresolved_buffer

(** Fully resolved buffer. Once [resolve_global_patches] has run, the
    bytes and relocation list are in their final form and can be
    consumed via [contents]/[relocations]. *)
type buffer

val size : buffer -> int

val relocations : buffer -> Relocation.t list

val assemble_section : arch -> section -> unresolved_buffer


(** Resolve every buffer's deferred data-directive patches using a
    global symbol table built from all the buffers' labels. Same-section
    symbol differences fold to literal bytes; external absolute
    references and supported current-section-anchored PC-relative
    references produce ELF relocations. Unsupported cross-section
    subtractions are rejected with a fatal error. Returns the same
    buffers with their type refined to [buffer]. *)
val resolve_global_patches : unresolved_buffer list -> buffer list

(** Assemble and resolve a single self-contained section in one step. Use this
    only when the section definitely has no cross-section references. *)
val assemble_singleton_section : arch -> section -> buffer

val get_symbol : buffer -> StringMap.key -> symbol

val contents_mut : buffer -> bytes

val contents : buffer -> string

val add_patch : offset:int -> size:data_size -> data:int64 -> buffer -> unit

val labels : buffer -> symbol String.Tbl.t

(** Module implementing Binary_emitter_intf.S for use by ocaml-jit *)
module For_jit :
  Binary_emitter_intf.S
    with type Assembled_section.t = buffer
     and type Relocation.t = Relocation.t
