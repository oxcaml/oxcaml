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

(** Partitioning object files into size-limited buckets.

    This module partitions a list of files with their sizes into buckets,
    where each bucket's total size is at most a configurable threshold. *)

(** Errors that can occur during file partitioning. *)
type error =
  | File_exceeds_partition_size of
      { filename : string;
        size : int64;
        threshold : int64
      }

(** Exception wrapper for partitioning errors. *)
exception Error of error

(** Pretty-print a partitioning error. *)
val report_error : Format.formatter -> error -> unit

(** Default partition size threshold in bytes (1 GiB). *)
val default_partition_size : int64

(** Convert gigabytes to bytes. *)
val bytes_of_gb : float -> int64

(** [partition_files ~threshold file_sizes] partitions files into buckets,
    starting a new bucket when adding the next file would exceed [threshold].
    The order of files is preserved.

    The input should only contain files destined for partitioning (OCaml files,
    startup object, cached genfns). Passthrough files (C stubs, runtime libs)
    should be handled separately by the caller.

    Raises [Error (File_exceeds_partition_size _)] if any single file exceeds
    the threshold. *)
val partition_files :
  threshold:int64 -> Measure_object_files.File_size.t list -> Partition.t list
