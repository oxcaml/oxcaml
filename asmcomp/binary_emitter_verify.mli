(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Jane Street Group LLC                             *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Verification of binary emitter output against system assembler output.

    This module compares the section contents produced by the binary emitter
    (saved in .binary-sections/ directories) against the corresponding sections
    extracted from object files produced by the system assembler.
*)

type section_mismatch =
  { section_name : string;
    byte_offset : int;  (** For .text, aligned to instruction boundary *)
    instruction_offset : int option;
    expected : string;  (** from binary emitter, hex dump *)
    actual : string;  (** from assembler, hex dump *)
    expected_size : int;
    actual_size : int
  }

type relocation_mismatch =
  { section_name : string;
    offset : int;
    expected : string;
    actual : string
  }

type mismatch =
  | Section_content of section_mismatch
  | Section_size of
      { section_name : string;
        expected : int;
        actual : int
      }
  | Relocation of relocation_mismatch
  | Missing_section of string
  | Missing_binary_sections_dir of string

type result =
  | Match of
      { text_size : int;
        data_size : int
      }
  | Mismatch of mismatch
  | Object_file_error of string

(** Compare binary emitter output against assembled object file.

    @param unix The Unix module (as first-class module)
    @param obj_file Path to the .o file produced by the system assembler
    @param binary_sections_dir Path to the .binary-sections/ directory
    @return Comparison result
*)
val compare :
  (module Compiler_owee.Unix_intf.S) ->
  obj_file:string ->
  binary_sections_dir:string ->
  result

(** Print a comparison result to a formatter. *)
val print_result : Format.formatter -> result -> unit
