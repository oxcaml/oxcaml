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

(** Dissector pass for analyzing ELF object files.

    The dissector analyzes all object files involved in a link to compute the
    total size of allocated ELF sections. This information is used to partition
    object files to prevent relocation overflow when linking very large
    executables with the small code model. *)

(** Errors that can occur during the dissector pass. *)
type error =
  | Measure_error of Measure_object_files.error
  | Partition_error of Partition_object_files.error
  | Partial_link_error of Partial_link.error
  | Nodynlink_incompatible

(** Exception wrapper for dissector errors. *)
exception Error of error

(** Pretty-print a dissector error. *)
val report_error : Format.formatter -> error -> unit

(** Result of running the dissector. After the dissector runs, OCaml object
    files (ml_objfiles, startup_obj) are baked into the partition files.
    Passthrough files (ccobjs, runtime_libs) bypass partial linking and are
    passed directly to the final linker.
    Use {!Build_linker_args.build} to convert this result into linker
    arguments. *)
module Result : sig
  type t

  (** Returns the partially-linked partition object files. *)
  val linked_partitions : t -> Partition.Linked.t list

  (** Returns the passthrough files that should be passed directly to the
      final linker without partial linking. These are C stub files and runtime
      libraries that may have sections like .gcc_except_table that don't work
      well with ld -r. *)
  val passthrough_files : t -> string list

  (** Returns the path to the generated linker script. *)
  val linker_script : t -> string
end

(** Run the dissector pass.

    Analyzes all object files that will be involved in a link, computes the
    total size of allocated ELF sections, partitions files to stay under the
    threshold, partially links each partition, and extracts relocations that
    need conversion to use an intermediate PLT or GOT.

    The startup_obj is analyzed like all other .o files but may need to be
    handled specially during partitioning.

    @param unix First-class Unix module for file operations
    @param temp_dir Directory for temporary and output files
    @param ml_objfiles The OCaml object files (.o, .a derived from .cmx, .cmxa)
    @param startup_obj The startup object file
    @param ccobjs Extra C object files from -cclib (Clflags.ccobjs)
    @param runtime_libs Runtime libraries (from runtime_lib ())
    @param cached_genfns Optional path to cached generic functions

    Raises [Error (Partition_error (File_exceeds_partition_size _))] if any
    single file exceeds the partition threshold.

    Raises [Error (Partial_link_error _)] if partial linking fails.

    Raises an error if Target_system is not Linux. *)
val run :
  unix:(module Compiler_owee.Unix_intf.S) ->
  temp_dir:string ->
  ml_objfiles:string list ->
  startup_obj:string ->
  ccobjs:string list ->
  runtime_libs:string list ->
  cached_genfns:string option ->
  Result.t
