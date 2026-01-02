(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

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

(** Functions for testing the verification logic itself. *)
module For_testing : sig
  (** Compare two object files directly using Owee extraction.
      Useful for testing the comparison logic without the binary emitter.

      @param expected_pathname Path to the "expected" object file
      @param actual_pathname Path to the "actual" object file to compare against
      @return Comparison result
  *)
  val compare_object_files :
    (module Compiler_owee.Unix_intf.S) ->
    expected_pathname:string ->
    actual_pathname:string ->
    result
end
