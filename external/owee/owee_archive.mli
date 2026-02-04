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

(** Reader for Unix ar archive files (.a static libraries).

    This module supports both:
    - System V/GNU format (used on Linux): symbol table "/", string table "//",
      and "/N" references for long filenames
    - BSD format (used on macOS): symbol table "__.SYMDEF" and "#1/N" prefix
      with inline long filenames

    The archive contains object files (typically ELF or Mach-O format) that can
    be individually accessed without loading the entire archive contents into
    memory. *)

(** An archive file that has been opened for reading. The underlying buffer is
    typically memory-mapped, so member contents are not loaded until accessed.
*)
type t

(** Metadata about a single member (object file) within the archive. Field order
    matches the ar header format. *)
type member = private
  { name : string;
        (** The filename of the member. Long filenames are resolved
            automatically (from the string table for System V/GNU, or inline for
            BSD). *)
    mtime : int;  (** Modification time as seconds since the Unix epoch. *)
    uid : int;  (** User ID of the file owner. *)
    gid : int;  (** Group ID of the file owner. *)
    mode : int;  (** File permission bits (octal). *)
    size : int;
        (** Size of the member content in bytes (not including the header). *)
    data_offset : int
        (** Absolute offset within the archive buffer where member data begins.
            This can be used with [Bigarray.Array1.sub] to access the data
            without copying. *)
  }

(** Alias for [member] for clarity when referring to header metadata. *)
type member_header = member

(** [read buf] reads an ar archive from the given buffer. Returns the archive
    handle and a list of members. The buffer should contain the entire archive
    (e.g., via memory mapping).

    Raises [Owee_buf.Invalid_format] if the buffer does not contain a valid ar
    archive. *)
val read : Owee_buf.t -> t * member list

(** [member_body archive member] returns a sub-buffer containing just the data
    for the given member. This does not copy the data; it returns a view into
    the original archive buffer.

    The returned buffer can be passed to [Owee_elf.read_elf] to parse the member
    as an ELF object file. *)
val member_body : t -> member -> Owee_buf.t

(** [iter_members archive members ~f] calls [f] on each member. This is a
    convenience function equivalent to [List.iter f members]. *)
val iter_members : t -> member list -> f:(member -> unit) -> unit
