(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
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

(** The post-link frame-descriptor index.

    A linked executable holds every frame descriptor of every unit it links in
    its [caml_frametables] section, and the startup object reserves a
    zero-filled [caml_frame_index] section (see [Frame_index_layout]). After
    linking, [build] decodes the frametables, sorts the descriptors by return
    address and writes a lookup structure into the reserved section, in place.
    The runtime then finds descriptors through the index without building a hash
    table at startup. *)

[@@@ocaml.warning "+a-40-41-42"]

type error

exception Error of error

(** The number of frame descriptors the given object files and archives
    contribute to a link, found by reading the count word of their
    [*__frametable] symbols: those of the listed units when there are any, and
    otherwise every one in the file. Files that do not exist, are not ELF, or
    are linker options contribute nothing. *)
val estimate_descriptors :
  (module Compiler_owee.Unix_intf.S) -> Linkenv.objfile_to_link list -> int

(** Fill in the index of the linked ELF executable [file]. Raises [Error] if the
    reservation is too small or the frametables are inconsistent, and does
    nothing if the reservation is header-only. *)
val build : (module Compiler_owee.Unix_intf.S) -> file:string -> unit

val report_error : Format_doc.formatter -> error -> unit
