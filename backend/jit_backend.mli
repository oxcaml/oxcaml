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

(** JIT backend dispatch - architecture-independent interface for JIT. This
    module allows jit.ml to register a callback without knowing about
    architecture-specific emitters. *)

module String_map : Map.S with type key = string

(** Packed sections with their Binary_emitter.S module, hiding the
    architecture-specific types using an existential. *)
type packed_sections =
  | Packed :
      { emitter :
          (module Binary_emitter_intf.S
             with type Assembled_section.t = 'a
              and type Relocation.t = 'r);
        sections : 'a String_map.t
      }
      -> packed_sections

type callback = packed_sections -> unit

(** Register a JIT callback. When code is generated, the callback will be
    invoked with the assembled sections packed with their emitter module.
    This handles architecture dispatch internally. *)
val register : callback -> unit

(** Unregister the JIT callback and restore previous state. *)
val unregister : unit -> unit
