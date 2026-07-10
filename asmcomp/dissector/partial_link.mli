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

(* CR mshinwell: This file needs to be code reviewed *)

(** Partial linking of object files.

    This module partially links groups of object files into single relocatable
    object files, to work around relocation overflow issues when linking very
    large executables with the small code model. *)

(** Errors that can occur during partial linking. *)
type error =
  | Linker_error of
      { partition_index : int;
        exit_code : int;
        files : string list
      }

(** Exception wrapper for partial linking errors. *)
exception Error of error

(** Pretty-print a partial linking error. *)
val report_error : Format_doc.formatter -> error -> unit

(** [link_one_partition unix ~temp_dir ~partition_index partition] partially
    links [partition] into a single relocatable object file.

    Creates a response file listing the input files, then invokes the linker
    with:
    {v   ld --whole-archive @<response_file> --relocatable -o <output.o> v}

    Returns the linked partition with the path to the output .o file. Raises
    [Error] if the link fails.

    @param temp_dir Directory for temporary and output files *)
val link_one_partition :
  (module Compiler_owee.Unix_intf.S) ->
  temp_dir:string ->
  partition_index:int ->
  Partition.t ->
  Partition.Linked.t

(** [link_all unix ~temp_dir ~max_parallelism ~init ~f partitions] partially
    links each partition as for [link_one_partition] and folds [f] over the
    linked partitions, in partition order, as their links complete.

    Up to [max_parallelism] linker child processes run concurrently; [f] runs in
    the driver while the started links proceed in the background. Raises [Error]
    if a link fails. If a link fails or [f] raises, the in-flight children are
    killed (SIGTERM) and reaped before the exception propagates.

    @param temp_dir Directory for temporary and output files *)
val link_all :
  (module Compiler_owee.Unix_intf.S) ->
  temp_dir:string ->
  max_parallelism:Misc.Maybe_bounded.t ->
  init:'acc ->
  f:('acc -> Partition.Linked.t -> 'acc) ->
  Partition.t list ->
  'acc
