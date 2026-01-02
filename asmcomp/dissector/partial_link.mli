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
val report_error : Format.formatter -> error -> unit

(** [link_partitions ~temp_dir partitions] partially links each partition into a
    single relocatable object file.

    For each partition, creates a response file listing the input files, then
    invokes the linker with:
    {v
      ld --whole-archive @<response_file> --relocatable -o <output.o>
    v}

    Returns the list of linked partitions with paths to the output .o files.

    @param temp_dir Directory for temporary and output files *)
val link_partitions :
  temp_dir:string -> Partition.t list -> Partition.Linked.t list
