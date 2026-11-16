(******************************************************************************
 *                                  OxCaml                                    *
 *                        Jacob Van Buren, Jane Street                        *
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

(** A codegen variant is the tuple of whole-program code generation axis
    values selected for a native compilation or link.  Units compiled for
    different variants must never be mixed within one image. *)

type t = { frame_pointers : bool }

(** The variant selected by the current invocation's flags.  Always
    [default] when the target is JSIR (js_of_ocaml or wasm_of_ocaml): the
    axes describe native code generation only, and the JSIR libraries exist
    only in the default library root. *)
val current : unit -> t

(** The variant baked in at configure time. *)
val default : t

val equal : t -> t -> bool

val is_default : t -> bool

(** Canonical name encoding absolute axis values ("fp" / "base"). *)
val to_string : t -> string

val of_string : string -> t option

(** The marker recorded in native compilation artifacts.  Besides the
    per-invocation axes of [t] it includes configure-time settings
    (address sanitizer) whose mixing across installs must also be
    rejected. *)
type fingerprint = (string * string) list

val fingerprint : t -> fingerprint

val current_fingerprint : unit -> fingerprint

val equal_fingerprint : fingerprint -> fingerprint -> bool

(** Renders e.g. "frame_pointers=true, address_sanitizer=false", for use
    in error messages and [ocamlobjinfo]. *)
val string_of_fingerprint : fingerprint -> string
