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

(* CR metaprogramming jrickard: This file has not been code reviewed *)

(* [eval] must be the first item in this file as transl emits lambda referencing
   it as element 0 in this module. *)

(** Evaluate a quoted OCaml expression at runtime. *)
val eval : CamlinternalQuote.Code.t -> Obj.t

module type Jit_intf = sig
  val jit_load :
    phrase_name:string ->
    Format.formatter ->
    Lambda.program ->
    (Obj.t, exn) Result.t

  val jit_lookup_symbol : string -> Obj.t option
end

(** Use the given JIT instead of the compiler's one. *)
val set_jit : (module Jit_intf) -> unit

(** Disallow the reading of bundles from the current executable. Instead, fetch
    them via the normal mechanisms used by compilerlibs. This should only be
    used if the compilerlibs state in the process is already set up with the
    correct [Load_path] information for .cmi and .cmx resolution (as is the case
    in mdx, for example). *)
val use_existing_compilerlibs_state_for_artifacts : unit -> unit
