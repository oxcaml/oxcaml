(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2026 Jane Street Group LLC                                       *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
[@@@ocaml.warning "+a-40-41-42"]

(** Identify functions in a compilation unit whose CFGs are equivalent (up to a
    substitution, see [Cfg_equiv]) to one already seen and rewrite the
    duplicates to a tail-call thunk that jumps to the representative.

    The module maintains a unit-scoped, mutable set of representatives. It is
    used as a streaming filter: each function passes through [process], and
    either becomes a fresh representative or is rewritten in place.

    [reset_unit_info] must be called at the start of every compilation unit. *)

val reset_unit_info : unit -> unit

(** [run fd_cmm cfg_with_layout] inspects [cfg_with_layout]:
    - if it is equivalent to a previously-registered representative, returns a
      freshly-built thunk CFG whose body is a single tail call to the
      representative's symbol;
    - otherwise registers [(fd_cmm, cfg_with_layout)] as a new representative
      and returns [cfg_with_layout] unchanged. *)
val run : Cmm.fundecl -> Cfg_with_layout.t -> Cfg_with_layout.t
