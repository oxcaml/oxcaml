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

(** Substitution type and helpers used by [Cfg_equiv]. *)

(* CR xclerc for xclerc: should `symbols` and `func_symbols` be merged? *)
type t =
  { labels : Label.t Label.Tbl.t;
        (** Total map on labels; fatal error on missing key. *)
    symbols : Cmm.symbol Cmm.Symbol_tbl.t;
        (** Partial map on [Cmm.symbol] values; identity on missing key. *)
    func_symbols : string Misc.Stdlib.String.Tbl.t
        (** Partial map on [func_symbol] strings from external calls; identity
            on missing key. *)
  }

val make : unit -> t

val subst_label : t -> Label.t -> Label.t

val subst_symbol : t -> Cmm.symbol -> Cmm.symbol

val subst_func_symbol : t -> string -> string
