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

(** Substitution type and helpers used by [Cfg_equiv].

    A substitution is a mutable record of three partial maps: one for labels,
    one for [Cmm.symbol] values, and one for the [func_symbol] strings of
    external calls. The label map is consulted as total (a missing key is a
    fatal error in [subst_label]); the other two default to identity on missing
    keys. *)

(* The [symbols] and [func_symbols] maps are intentionally kept separate.
   [symbols] holds [Cmm.symbol] values (with their [is_global] flag) and is used
   for OCaml-side symbol references: [Const_symbol], direct/indirect callees,
   and the symbol part of [Ibased] addressing modes. [func_symbols] holds the
   raw strings naming external (C/runtime) functions in external-call
   operations. Merging them would force one representation: a string-only key
   would lose [is_global], while a [Cmm.symbol] key would require fabricating an
   [is_global] for the external case. The two also map distinct domains. *)

(* CR xclerc: it is unclear whether [func_symbols] is actually needed. The
   intended use of [Cfg_equiv] is to merge OCaml functions whose CFGs coincide,
   which calls for substituting OCaml-side symbol references but probably not
   the names of external runtime functions. If no caller needs this, the field
   and its helpers can be removed. *)
type t

val make : unit -> t

(** [add_label subst from_lbl to_lbl] records that [from_lbl] is replaced by
    [to_lbl]. If [from_lbl] is already mapped to a label equal to [to_lbl], the
    call is a no-op; if it is already mapped to a different label, a fatal error
    is raised. *)
val add_label : t -> Label.t -> Label.t -> unit

(** Same as [add_label] for symbols. *)
val add_symbol : t -> Cmm.symbol -> Cmm.symbol -> unit

(** Same as [add_label] for the [func_symbol] strings of external calls. *)
val add_func_symbol : t -> string -> string -> unit

val subst_label : t -> Label.t -> Label.t

val subst_symbol : t -> Cmm.symbol -> Cmm.symbol

val subst_func_symbol : t -> string -> string
