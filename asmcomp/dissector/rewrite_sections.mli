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

(** Rewrite ELF sections for the dissector.

    This module rewrites a partially-linked object file to add IGOT and IPLT
    sections, their associated relocation sections, and rewrites the .rela.text
    section to redirect PLT32 and GOTPCRELX relocations through the IGOT/IPLT.

    For Large_code partitions, sections are also renamed with a prefix
    (e.g., .text -> .caml.p1.text) so the linker script can place them at
    higher addresses.

    The transformation adds:

    - .data.igot section with R_X86_64_64 relocations to original symbols

    - .text.iplt section with R_X86_64_PC32 relocations to IGOT entries

    - Synthetic symbols for each IGOT and IPLT entry

    - Modified .rela.text entries pointing to IPLT/IGOT symbols instead of
      original external symbols *)

(** [rewrite unix ~input_file ~output_file ~partition_kind ~igot_and_iplt
    ~relocations] reads the ELF object file at [input_file], adds IGOT and
    IPLT sections, rewrites relocations, and writes the result to
    [output_file].

    For [Large_code] partitions, section names are also prefixed (e.g.,
    .text -> .caml.p1.text). [Main] partition sections keep their original
    names.

    @param unix First-class Unix module for file operations
    @param input_file Path to the input partially-linked object file
    @param output_file Path to write the rewritten object file
    @param partition_kind The kind of partition (Main or Large_code)
    @param igot_and_iplt The IGOT and IPLT structures to add
    @param relocations The extracted relocations identifying which entries
      need rewriting *)
val rewrite :
  (module Compiler_owee.Unix_intf.S) ->
  input_file:string ->
  output_file:string ->
  partition_kind:Partition.kind ->
  igot_and_iplt:Build_igot_and_iplt.t ->
  relocations:Extract_relocations.t ->
  unit
