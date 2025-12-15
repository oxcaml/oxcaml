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

(** Measuring allocated section sizes in object files.

    This module computes the total size of allocated ELF sections across
    a collection of object files, archives, and OCaml compilation units. *)

(** Errors that can occur when measuring object files. *)
type error =
  | File_not_found of string
  | Duplicate_file of string

(** Exception wrapper for measurement errors. *)
exception Error of error

(** Pretty-print a measurement error. *)
val report_error : Format.formatter -> error -> unit

(** The origin of a file, used to determine partition placement. *)
type file_origin =
  | OCaml  (** OCaml-compiled code (.o from .cmx, .a from .cmxa) *)
  | C_stub  (** C stub libraries (from -cclib or lib_ccobjs) *)
  | Runtime  (** Runtime library (libasmrun) *)
  | Startup  (** Startup object file *)
  | Cached_genfns  (** Cached generic functions *)

(** Information about a single file's allocated section size. *)
module File_size : sig
  type t

  (** Returns the filename. *)
  val filename : t -> string

  (** Returns the total size of allocated sections in the file. *)
  val size : t -> int64

  (** Returns whether the file contains a .probes section. *)
  val has_probes : t -> bool

  (** Returns the origin of the file. *)
  val origin : t -> file_origin
end

(** [measure_files unix ~files] computes the allocated section size for each
    file in [files].

    Handles the following file types based on extension:

    - .o: ELF object file, analyzed directly

    - .a: archive file, all .o members analyzed and summed

    - .cmx: finds associated .o file (same basename)

    - .cmxa: finds associated .a file, plus any lib_ccobjs

    Files are tracked to avoid double-counting when the same file appears
    multiple times or is referenced transitively. Returns an empty entry for
    files with unrecognized extensions.

    Each file is paired with its origin, which determines partition
    placement. *)
val measure_files :
  (module Compiler_owee.Unix_intf.S) ->
  files:(string * file_origin) list ->
  File_size.t list
