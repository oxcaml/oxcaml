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

type patch_size =
  | P8
  | P16
  | P32
  | P64

type t

val create : unit -> t

val buffer : t -> Buffer.t

val offset_in_bytes : t -> int

val set_offset_in_bytes : t -> int -> unit

val add_relocation_at_current_offset :
  t -> symbol_name:string -> reloc_kind:Relocation.Kind.t -> unit

val define_symbol : t -> string -> unit

val define_label : t -> string -> unit

val find_symbol_offset_in_bytes : t -> string -> int option

val find_label_offset_in_bytes : t -> string -> int option

val find_nearest_symbol_before : t -> int -> (string * int) option

val find_symbol_or_label_offset_in_bytes : t -> string -> int option

val relocations : t -> Relocation.t list

val symbols : t -> (string, int) Hashtbl.t

val labels : t -> (string, int) Hashtbl.t

val add_patch : t -> offset:int -> size:patch_size -> data:int64 -> unit

val contents_mut : t -> bytes

val contents : t -> string
